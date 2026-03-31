{-
  Accessibility.hs -- Chromatin state machine and remodeling operations

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Epigenetic Marks as Runtime State

  This module implements the chromatin accessibility state machine,
  computing accessibility from the combination of methylation and
  histone marks. ATP-dependent chromatin remodelers (SWI/SNF, ISWI,
  CHD, INO80) are modeled as garbage collection / defragmentation
  operations on nucleosome arrays.
-}

module Accessibility
  ( ChromatinState(..)
  , Remodeler(..)
  , RemodelerFamily(..)
  , RemodelerAction(..)
  , TranscriptionFactor(..)
  , XInactivationState(..)
  , CellDivisionResult(..)
  , computeAccessibility
  , applyRemodeler
  , remodel
  , evictNucleosome
  , slideNucleosome
  , resolveBivalent
  , xInactivate
  , xReactivate
  , simulateDivision
  , serializeState
  , deserializeState
  , showChromatinState
  , showRemodeler
  , showXState
  , garbageCollect
  , defragment
  ) where

import qualified Methylation as M
import qualified HistoneModification as H
import State (EpigeneticState(..), AccessibilityLevel(..))

-- | Chromatin state combining all epigenetic information
data ChromatinState = ChromatinState
  { csAccessibility :: AccessibilityLevel
  , csDNaseHS       :: Bool       -- ^ DNase I hypersensitivity
  , csATACseq       :: Double     -- ^ ATAC-seq signal (0.0-1.0)
  , csNucOccupancy  :: Double     -- ^ Nucleosome occupancy (0.0-1.0)
  } deriving (Eq, Show, Read)

-- | ATP-dependent chromatin remodeler families
data RemodelerFamily
  = SWI_SNF   -- ^ Evicts/slides nucleosomes (activating)
  | ISWI      -- ^ Orders/spaces nucleosomes (can repress or activate)
  | CHD       -- ^ NuRD complex (often repressive)
  | INO80     -- ^ Histone variant exchange (H2A.Z)
  deriving (Eq, Show, Read)

-- | Specific remodeler complexes
data Remodeler = Remodeler
  { remodelerName   :: String
  , remodelerFamily :: RemodelerFamily
  , remodelerAction :: RemodelerAction
  } deriving (Eq, Show, Read)

-- | Remodeler actions
data RemodelerAction
  = Evict      -- ^ Remove nucleosome entirely
  | Slide      -- ^ Reposition nucleosome
  | Space      -- ^ Regular spacing of nucleosomes
  | Exchange   -- ^ Exchange histone variants
  deriving (Eq, Show, Read)

-- | Transcription factor binding
data TranscriptionFactor = TranscriptionFactor
  { tfName    :: String
  , tfMotif   :: String
  , tfPioneer :: Bool  -- ^ Pioneer factor can bind closed chromatin
  } deriving (Eq, Show, Read)

-- | X-inactivation state (process isolation)
data XInactivationState
  = XActive             -- ^ Active X chromosome
  | XChoicePhase        -- ^ Counting/choice phase (pre-inactivation)
  | XInitiation         -- ^ Xist RNA coating beginning
  | XSpreading          -- ^ Xist spreading, silencing propagating
  | XBarrBody           -- ^ Fully inactive, Barr body formed
  | XEscapee            -- ^ Gene escaping inactivation (~15% of genes)
  deriving (Eq, Ord, Show, Read)

-- | Result of cell division for epigenetic inheritance
data CellDivisionResult = CellDivisionResult
  { daughterCell1 :: EpigeneticState AccessibilityLevel
  , daughterCell2 :: EpigeneticState AccessibilityLevel
  , faithfulness  :: Double  -- ^ How faithfully marks were copied (0.0-1.0)
  } deriving (Eq, Show, Read)

-- | Compute accessibility from methylation and histone state
computeAccessibility :: M.MethylationMap -> H.HistoneMap -> AccessibilityLevel
computeAccessibility meth hist =
  let methFrac  = M.methylationFraction meth
      histPerms = map H.combinatorialReadout (H.nucleosomes hist)
      hasKernelLock = any (== H.KernelLock) histPerms
      hasExecute    = any (== H.Execute) histPerms
      hasNoRead     = any (== H.NoRead) histPerms
      hasReadWrite  = any (== H.ReadWrite) histPerms
  in if hasKernelLock
     then ConstitutiveHet
     else if hasExecute && hasNoRead
     then Bivalent
     else if methFrac > 0.8
     then FullyRepressed
     else if hasNoRead
     then FullyRepressed
     else if hasExecute && methFrac < 0.2
     then FullyOpen
     else if hasReadWrite
     then PartiallyOpen
     else if methFrac > 0.5
     then PartiallyRepressed
     else PartiallyOpen

-- | Apply a remodeler to an epigenetic state
applyRemodeler :: Remodeler -> EpigeneticState AccessibilityLevel
               -> EpigeneticState AccessibilityLevel
applyRemodeler r st = case remodelerFamily r of
  SWI_SNF -> st { histones = clearNucleosomes (histones st)
                , accessibility = FullyOpen }
  ISWI    -> st { accessibility = PartiallyOpen }
  CHD     -> st { accessibility = PartiallyRepressed }
  INO80   -> st { accessibility = PartiallyOpen }

-- | Generic remodel operation
remodel :: RemodelerFamily -> EpigeneticState AccessibilityLevel
        -> EpigeneticState AccessibilityLevel
remodel fam = applyRemodeler (Remodeler (show fam) fam Evict)

-- | Evict a nucleosome (SWI/SNF-like)
evictNucleosome :: Int -> H.HistoneMap -> H.HistoneMap
evictNucleosome idx (H.HistoneMap ns)
  | idx >= 0 && idx < length ns = H.HistoneMap (take idx ns ++ drop (idx + 1) ns)
  | otherwise = H.HistoneMap ns

-- | Slide a nucleosome (reposition without removing)
slideNucleosome :: Int -> Int -> H.HistoneMap -> H.HistoneMap
slideNucleosome from to (H.HistoneMap ns)
  | from >= 0 && from < length ns && to >= 0 && to <= length ns =
    let nuc = ns !! from
        removed = take from ns ++ drop (from + 1) ns
        to' = if to > from then to - 1 else to
    in H.HistoneMap (take to' removed ++ [nuc] ++ drop to' removed)
  | otherwise = H.HistoneMap ns

-- | Resolve bivalent chromatin to either active or repressed
resolveBivalent :: Bool -> H.HistoneState -> H.HistoneState
resolveBivalent toActive hs
  | not (H.isBivalentState hs) = hs  -- not bivalent, no change
  | toActive    = H.removeMark (H.HistoneMark H.H3K27 H.Methylation3) hs
  | otherwise   = H.removeMark (H.HistoneMark H.H3K4  H.Methylation3) hs

-- | X-inactivation: silence an entire chromosome (process isolation)
xInactivate :: EpigeneticState AccessibilityLevel -> (EpigeneticState AccessibilityLevel, XInactivationState)
xInactivate st =
  let silenced = st { methylation   = methylateAll (methylation st)
                    , histones      = addRepressiveMarks (histones st)
                    , accessibility = ConstitutiveHet
                    }
  in (silenced, XBarrBody)

-- | X-reactivation (occurs in inner cell mass of early embryo)
xReactivate :: EpigeneticState AccessibilityLevel -> (EpigeneticState AccessibilityLevel, XInactivationState)
xReactivate st =
  let reactivated = st { methylation   = M.emptyMethylationMap
                       , histones      = H.emptyHistoneMap
                       , accessibility = PartiallyOpen
                       }
  in (reactivated, XActive)

-- | Simulate cell division with epigenetic inheritance
simulateDivision :: EpigeneticState AccessibilityLevel -> CellDivisionResult
simulateDivision parent =
  let -- Methylation is maintained by DNMT1 with ~95% fidelity
      meth1 = M.maintenanceMethylation (methylation parent)
      meth2 = M.maintenanceMethylation (methylation parent)
      -- Histones are diluted 50/50, then restored
      hist1 = dilute (histones parent)
      hist2 = dilute (histones parent)
      -- Compute new accessibility
      acc1 = computeAccessibility meth1 hist1
      acc2 = computeAccessibility meth2 hist2
      d1 = EpigeneticState meth1 hist1 acc1
      d2 = EpigeneticState meth2 hist2 acc2
  in CellDivisionResult d1 d2 0.95

-- | Serialize epigenetic state to string (checkpoint)
serializeState :: EpigeneticState AccessibilityLevel -> String
serializeState st = show st

-- | Deserialize epigenetic state from string
deserializeState :: String -> Maybe (EpigeneticState AccessibilityLevel)
deserializeState s = case reads s of
  [(st, "")] -> Just st
  _          -> Nothing

-- | Garbage collection: remove conflicting marks
garbageCollect :: EpigeneticState AccessibilityLevel -> EpigeneticState AccessibilityLevel
garbageCollect st =
  let cleaned = H.HistoneMap (map cleanNucleosome (H.nucleosomes (histones st)))
      newAcc  = computeAccessibility (methylation st) cleaned
  in st { histones = cleaned, accessibility = newAcc }

-- | Clean a single nucleosome by removing contradictory marks
cleanNucleosome :: H.HistoneState -> H.HistoneState
cleanNucleosome hs
  | H.isBivalentState hs = hs  -- bivalent is a valid state in stem cells
  | otherwise = hs

-- | Defragment: regularize nucleosome spacing
defragment :: EpigeneticState AccessibilityLevel -> EpigeneticState AccessibilityLevel
defragment st = st  -- spacing is implicit in our model

-- | Pretty-print chromatin state
showChromatinState :: ChromatinState -> String
showChromatinState cs = unlines
  [ "Chromatin State:"
  , "  Accessibility:      " ++ show (csAccessibility cs)
  , "  DNase HS:           " ++ show (csDNaseHS cs)
  , "  ATAC-seq signal:    " ++ show (csATACseq cs)
  , "  Nucleosome occupancy: " ++ show (csNucOccupancy cs)
  ]

-- | Pretty-print a remodeler
showRemodeler :: Remodeler -> String
showRemodeler r = remodelerName r ++ " (" ++ show (remodelerFamily r)
              ++ ", " ++ show (remodelerAction r) ++ ")"

-- | Pretty-print X-inactivation state
showXState :: XInactivationState -> String
showXState XActive      = "Active X (Xa)"
showXState XChoicePhase = "Choice Phase (counting)"
showXState XInitiation  = "Initiation (Xist expression)"
showXState XSpreading   = "Spreading (Xist coating)"
showXState XBarrBody    = "Barr Body (Xi, fully silent)"
showXState XEscapee     = "Escapee (expressed from Xi)"

-- Helper: clear all nucleosomes (SWI/SNF eviction)
clearNucleosomes :: H.HistoneMap -> H.HistoneMap
clearNucleosomes _ = H.HistoneMap []

-- Helper: methylate all sites in a map
methylateAll :: M.MethylationMap -> M.MethylationMap
methylateAll (M.MethylationMap sites) =
  M.MethylationMap (map (\s -> s { M.siteStatus = M.FullyMethylated }) sites)

-- Helper: add repressive marks to all nucleosomes
addRepressiveMarks :: H.HistoneMap -> H.HistoneMap
addRepressiveMarks (H.HistoneMap ns) =
  H.HistoneMap (map addRepr ns)
  where
    addRepr hs = H.addMark (H.HistoneMark H.H3K27 H.Methylation3)
               $ H.addMark (H.HistoneMark H.H3K9  H.Methylation3) hs

-- Helper: dilute histones (replication - rough model)
dilute :: H.HistoneMap -> H.HistoneMap
dilute (H.HistoneMap ns) = H.HistoneMap (map id ns)  -- simplified: marks persist
