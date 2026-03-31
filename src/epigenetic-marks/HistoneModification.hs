{-
  HistoneModification.hs -- Histone marks, writer/eraser enzymes, combinatorial readout

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Epigenetic Marks as Runtime State

  This module models histone post-translational modifications as
  access permission flags on chromatin regions. The histone code
  hypothesis maps to a type-theoretic permission system:

    H3K4me3  = chmod +x  (active promoter, execute permission)
    H3K27me3 = chmod -r  (Polycomb repression, read revoked)
    H3K9me3  = kernel lock (constitutive heterochromatin)
    H3K27ac  = public access (active enhancer)
-}

module HistoneModification
  ( HistoneMark(..)
  , HistonePosition(..)
  , ModificationType(..)
  , HistoneState(..)
  , HistoneMap(..)
  , WriterEnzyme(..)
  , EraserEnzyme(..)
  , ReaderDomain(..)
  , Permission(..)
  , emptyHistoneMap
  , combineHistoneMaps
  , showHistoneMap
  , addMark
  , removeMark
  , hasMark
  , getMarks
  , applyWriter
  , applyEraser
  , readMarks
  , combinatorialReadout
  , markToPermission
  , isActiveMark
  , isRepressiveMark
  , isBivalentState
  , showMark
  , showPermission
  , showHistoneState
  , allActiveMarks
  , allRepressiveMarks
  ) where

-- | Histone residue positions
data HistonePosition
  = H3K4    -- ^ Histone H3 Lysine 4
  | H3K9    -- ^ Histone H3 Lysine 9
  | H3K27   -- ^ Histone H3 Lysine 27
  | H3K36   -- ^ Histone H3 Lysine 36
  | H3K79   -- ^ Histone H3 Lysine 79
  | H4K20   -- ^ Histone H4 Lysine 20
  | H3K14   -- ^ Histone H3 Lysine 14 (acetylation)
  deriving (Eq, Ord, Show, Read)

-- | Types of histone modifications
data ModificationType
  = Methylation1  -- ^ Monomethylation (me1)
  | Methylation2  -- ^ Dimethylation (me2)
  | Methylation3  -- ^ Trimethylation (me3)
  | Acetylation   -- ^ Acetylation (ac)
  | Phosphorylation -- ^ Phosphorylation (ph)
  | Ubiquitination  -- ^ Ubiquitination (ub)
  deriving (Eq, Ord, Show, Read)

-- | A specific histone mark: position + modification
data HistoneMark = HistoneMark
  { markPosition     :: HistonePosition
  , markModification :: ModificationType
  } deriving (Eq, Ord, Show, Read)

-- | State of a single histone octamer
data HistoneState = HistoneState
  { histoneMarks :: [HistoneMark]
  } deriving (Eq, Show, Read)

-- | Map of histone states across a region (list of nucleosome states)
data HistoneMap = HistoneMap
  { nucleosomes :: [HistoneState]
  } deriving (Eq, Show, Read)

-- | Writer enzymes: add histone marks
data WriterEnzyme
  = MLL1_2     -- ^ Writes H3K4me3 (COMPASS complex)
  | SET1       -- ^ Writes H3K4me3
  | EZH2       -- ^ Writes H3K27me3 (PRC2 component)
  | SUV39H1    -- ^ Writes H3K9me3
  | P300_CBP   -- ^ Writes H3K27ac (also H3K14ac)
  | DOT1L      -- ^ Writes H3K79me
  | SETD2      -- ^ Writes H3K36me3
  deriving (Eq, Show, Read)

-- | Eraser enzymes: remove histone marks
data EraserEnzyme
  = KDM5A_B    -- ^ Erases H3K4me3 (JARID1A/B)
  | KDM6A_B    -- ^ Erases H3K27me3 (UTX/JMJD3)
  | KDM4A      -- ^ Erases H3K9me3
  | HDAC1_2    -- ^ Erases acetylation marks
  | KDM2A      -- ^ Erases H3K36me
  deriving (Eq, Show, Read)

-- | Reader domains: recognize specific marks
data ReaderDomain
  = Chromo     -- ^ Chromodomain (reads methylation, e.g., HP1 reads H3K9me3)
  | Tudor      -- ^ Tudor domain (reads methylation)
  | PHD        -- ^ PHD finger (reads H3K4me3)
  | Bromo      -- ^ Bromodomain (reads acetylation)
  | PWWP       -- ^ PWWP domain (reads H3K36me3)
  | WD40       -- ^ WD40 repeat (EED reads H3K27me3)
  deriving (Eq, Show, Read)

-- | Permission levels derived from histone marks
data Permission
  = Execute    -- ^ Active promoter (H3K4me3) -- chmod +x
  | ReadWrite  -- ^ Active enhancer (H3K27ac) -- public access
  | ReadOnly   -- ^ Poised state (H3K4me1)
  | NoRead     -- ^ Polycomb repressed (H3K27me3) -- chmod -r
  | KernelLock -- ^ Heterochromatin (H3K9me3) -- kernel-level lock
  deriving (Eq, Ord, Show, Read)

-- Canonical marks
h3k4me3, h3k27me3, h3k9me3, h3k27ac, h3k36me3, h3k4me1 :: HistoneMark
h3k4me3  = HistoneMark H3K4  Methylation3
h3k27me3 = HistoneMark H3K27 Methylation3
h3k9me3  = HistoneMark H3K9  Methylation3
h3k27ac  = HistoneMark H3K27 Acetylation
h3k36me3 = HistoneMark H3K36 Methylation3
h3k4me1  = HistoneMark H3K4  Methylation1

-- | Empty histone map
emptyHistoneMap :: HistoneMap
emptyHistoneMap = HistoneMap []

-- | Combine two histone maps
combineHistoneMaps :: HistoneMap -> HistoneMap -> HistoneMap
combineHistoneMaps (HistoneMap n1) (HistoneMap n2) = HistoneMap (n1 ++ n2)

-- | Pretty-print a histone map
showHistoneMap :: HistoneMap -> String
showHistoneMap (HistoneMap []) = "(no nucleosomes)"
showHistoneMap (HistoneMap ns) =
  show (length ns) ++ " nucleosomes, marks: ["
  ++ concatMap (\n -> concatMap (\m -> showMark m ++ " ") (histoneMarks n)) ns
  ++ "]"

-- | Add a mark to a histone state
addMark :: HistoneMark -> HistoneState -> HistoneState
addMark mark hs
  | mark `elem` histoneMarks hs = hs
  | otherwise = hs { histoneMarks = mark : histoneMarks hs }

-- | Remove a mark from a histone state
removeMark :: HistoneMark -> HistoneState -> HistoneState
removeMark mark hs =
  hs { histoneMarks = filter (/= mark) (histoneMarks hs) }

-- | Check if a histone state has a specific mark
hasMark :: HistoneMark -> HistoneState -> Bool
hasMark mark hs = mark `elem` histoneMarks hs

-- | Get all marks on a histone state
getMarks :: HistoneState -> [HistoneMark]
getMarks = histoneMarks

-- | Apply a writer enzyme to a histone state
applyWriter :: WriterEnzyme -> HistoneState -> HistoneState
applyWriter MLL1_2   = addMark h3k4me3
applyWriter SET1     = addMark h3k4me3
applyWriter EZH2     = addMark h3k27me3
applyWriter SUV39H1  = addMark h3k9me3
applyWriter P300_CBP = addMark h3k27ac
applyWriter DOT1L    = addMark (HistoneMark H3K79 Methylation3)
applyWriter SETD2    = addMark h3k36me3

-- | Apply an eraser enzyme to a histone state
applyEraser :: EraserEnzyme -> HistoneState -> HistoneState
applyEraser KDM5A_B = removeMark h3k4me3
applyEraser KDM6A_B = removeMark h3k27me3
applyEraser KDM4A   = removeMark h3k9me3
applyEraser HDAC1_2 = removeMark h3k27ac
applyEraser KDM2A   = removeMark h3k36me3

-- | Read marks using a reader domain, return recognized marks
readMarks :: ReaderDomain -> HistoneState -> [HistoneMark]
readMarks Chromo hs = filter (\m -> markModification m `elem` [Methylation1, Methylation2, Methylation3]
                                 && markPosition m == H3K9) (histoneMarks hs)
readMarks Tudor hs  = filter (\m -> markModification m `elem` [Methylation1, Methylation2, Methylation3]) (histoneMarks hs)
readMarks PHD hs    = filter (== h3k4me3) (histoneMarks hs)
readMarks Bromo hs  = filter (\m -> markModification m == Acetylation) (histoneMarks hs)
readMarks PWWP hs   = filter (== h3k36me3) (histoneMarks hs)
readMarks WD40 hs   = filter (== h3k27me3) (histoneMarks hs)

-- | Combinatorial readout: determine overall permission from all marks
combinatorialReadout :: HistoneState -> Permission
combinatorialReadout hs
  | hasMark h3k9me3 hs                          = KernelLock
  | hasMark h3k4me3 hs && hasMark h3k27me3 hs   = NoRead  -- bivalent resolves conservatively
  | hasMark h3k27me3 hs                          = NoRead
  | hasMark h3k4me3 hs && hasMark h3k27ac hs     = Execute
  | hasMark h3k4me3 hs                           = Execute
  | hasMark h3k27ac hs                           = ReadWrite
  | hasMark h3k4me1 hs                           = ReadOnly
  | otherwise                                     = ReadOnly

-- | Map a single mark to its closest permission analogy
markToPermission :: HistoneMark -> Permission
markToPermission m
  | m == h3k4me3  = Execute    -- chmod +x
  | m == h3k27me3 = NoRead     -- chmod -r
  | m == h3k9me3  = KernelLock -- kernel lock
  | m == h3k27ac  = ReadWrite  -- public access
  | m == h3k36me3 = ReadWrite  -- gene body (active transcription)
  | otherwise     = ReadOnly

-- | Check if a mark is activating
isActiveMark :: HistoneMark -> Bool
isActiveMark m = m `elem` [h3k4me3, h3k27ac, h3k36me3, h3k4me1]

-- | Check if a mark is repressive
isRepressiveMark :: HistoneMark -> Bool
isRepressiveMark m = m `elem` [h3k27me3, h3k9me3]

-- | Check if a histone state is bivalent (both H3K4me3 and H3K27me3)
isBivalentState :: HistoneState -> Bool
isBivalentState hs = hasMark h3k4me3 hs && hasMark h3k27me3 hs

-- | All active marks
allActiveMarks :: [HistoneMark]
allActiveMarks = [h3k4me3, h3k27ac, h3k36me3, h3k4me1]

-- | All repressive marks
allRepressiveMarks :: [HistoneMark]
allRepressiveMarks = [h3k27me3, h3k9me3]

-- | Pretty-print a histone mark
showMark :: HistoneMark -> String
showMark (HistoneMark H3K4  Methylation1)  = "H3K4me1"
showMark (HistoneMark H3K4  Methylation3)  = "H3K4me3"
showMark (HistoneMark H3K9  Methylation3)  = "H3K9me3"
showMark (HistoneMark H3K27 Methylation3)  = "H3K27me3"
showMark (HistoneMark H3K27 Acetylation)   = "H3K27ac"
showMark (HistoneMark H3K36 Methylation3)  = "H3K36me3"
showMark (HistoneMark H3K79 Methylation3)  = "H3K79me3"
showMark (HistoneMark H4K20 Methylation3)  = "H4K20me3"
showMark (HistoneMark pos mod_) = show pos ++ showModShort mod_

showModShort :: ModificationType -> String
showModShort Methylation1    = "me1"
showModShort Methylation2    = "me2"
showModShort Methylation3    = "me3"
showModShort Acetylation     = "ac"
showModShort Phosphorylation = "ph"
showModShort Ubiquitination  = "ub"

-- | Pretty-print a permission
showPermission :: Permission -> String
showPermission Execute    = "Execute (chmod +x, active promoter)"
showPermission ReadWrite  = "ReadWrite (public access, active enhancer)"
showPermission ReadOnly   = "ReadOnly (poised/unmarked)"
showPermission NoRead     = "NoRead (chmod -r, Polycomb repressed)"
showPermission KernelLock = "KernelLock (heterochromatin, HP1-bound)"

-- | Pretty-print a histone state
showHistoneState :: HistoneState -> String
showHistoneState (HistoneState []) = "(unmarked nucleosome)"
showHistoneState (HistoneState ms) =
  "[" ++ unwords (map showMark ms) ++ "] -> "
  ++ showPermission (combinatorialReadout (HistoneState ms))
