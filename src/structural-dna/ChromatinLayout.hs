{-
  ChromatinLayout.hs — TAD modeling, chromatin loops, and compartment assignment

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Structural DNA as Memory Architecture

  Topologically Associating Domains (TADs) are modeled as memory segments
  with CTCF/cohesin boundaries acting as segment delimiters. Chromatin
  loops model enhancer-promoter contacts as pointer dereferencing, and
  A/B compartments form a two-level memory hierarchy.
-}

module ChromatinLayout
  ( -- * TAD types
    TAD(..)
  , TADBoundary(..)
  , BoundaryProtein(..)
    -- * Loop types
    , ChromatinLoop(..)
  , LoopAnchor(..)
  , AnchorType(..)
    -- * Compartment types
  , CompartmentMap
  , InteractionFreq
    -- * TAD construction and operations
  , mkTAD
  , mkBoundary
  , tadSize
  , tadContains
  , isInsulated
  , interTADContact
    -- * Loop operations
  , mkLoop
  , loopSpan
  , isEnhancerPromoterLoop
  , dereferenceLoop
    -- * Compartment operations
  , assignCompartment
  , buildCompartmentMap
  , isCompartmentA
  , isCompartmentB
    -- * TAD hierarchy
  , SubTAD(..)
  , nestTADs
  , tadTree
    -- * Contact matrix
  , ContactMatrix
  , buildContactMatrix
  , lookupContact
  , matrixToList
    -- * Display
  , showTAD
  , showLoop
  , showCompartmentMap
  , showContactMatrix
  ) where

import Structure (Compartment(..), GenomicRegion(..), RegionType(..), AccessLevel(..))

-- | Proteins that define TAD boundaries
data BoundaryProtein
  = CTCF            -- ^ CCCTC-binding factor
  | Cohesin         -- ^ Cohesin ring complex
  | CTCFCohesin     -- ^ Both CTCF and cohesin (convergent CTCF motifs)
  deriving (Show, Eq, Ord)

-- | A TAD boundary element
data TADBoundary = TADBoundary
  { boundaryPosition :: Int
  , boundaryProtein  :: BoundaryProtein
  , boundaryStrength :: Double       -- ^ Insulation score (0-1)
  , convergentMotif  :: Bool         -- ^ Convergent CTCF motif orientation
  } deriving (Show, Eq)

-- | A topologically associating domain
data TAD = TAD
  { tadName        :: String
  , tadChromosome  :: Int
  , tadStart       :: Int
  , tadEnd         :: Int
  , tadLeftBound   :: TADBoundary
  , tadRightBound  :: TADBoundary
  , tadLoops       :: [ChromatinLoop]
  , tadCompartment :: Compartment
  , tadRegions     :: [GenomicRegion]
  } deriving (Show, Eq)

-- | Anchor point for a chromatin loop
data AnchorType
  = EnhancerAnchor     -- ^ Loop anchored at enhancer
  | PromoterAnchor     -- ^ Loop anchored at promoter
  | CTCFAnchor         -- ^ Loop anchored at CTCF site
  | StructuralAnchor   -- ^ Structural loop (no regulatory function)
  deriving (Show, Eq, Ord)

-- | A loop anchor with position and type
data LoopAnchor = LoopAnchor
  { anchorPosition :: Int
  , anchorType     :: AnchorType
  , anchorName     :: String
  } deriving (Show, Eq)

-- | A chromatin loop connecting two anchors
data ChromatinLoop = ChromatinLoop
  { loopName     :: String
  , loopAnchorA  :: LoopAnchor       -- ^ Source anchor (e.g., enhancer)
  , loopAnchorB  :: LoopAnchor       -- ^ Target anchor (e.g., promoter)
  , loopCohesin  :: Bool             -- ^ Mediated by cohesin
  , loopStrength :: Double           -- ^ Contact frequency (normalized)
  } deriving (Show, Eq)

-- | Sub-TAD for hierarchical nesting
data SubTAD = SubTAD
  { subTAD     :: TAD
  , subChildren :: [SubTAD]
  } deriving (Show, Eq)

-- | Compartment map: region name to compartment
type CompartmentMap = [(String, Compartment)]

-- | Interaction frequency between two positions
type InteractionFreq = Double

-- | Contact matrix for Hi-C data representation
type ContactMatrix = [((Int, Int), InteractionFreq)]

-- | Create a TAD
mkTAD :: String -> Int -> Int -> Int -> Compartment -> TAD
mkTAD name chrom start end comp = TAD
  { tadName        = name
  , tadChromosome  = chrom
  , tadStart       = start
  , tadEnd         = end
  , tadLeftBound   = mkBoundary start CTCFCohesin 0.9 True
  , tadRightBound  = mkBoundary end CTCFCohesin 0.9 True
  , tadLoops       = []
  , tadCompartment = comp
  , tadRegions     = []
  }

-- | Create a boundary
mkBoundary :: Int -> BoundaryProtein -> Double -> Bool -> TADBoundary
mkBoundary = TADBoundary

-- | Size of a TAD in base pairs
tadSize :: TAD -> Int
tadSize t = tadEnd t - tadStart t

-- | Check if a position falls within a TAD
tadContains :: TAD -> Int -> Bool
tadContains t pos = pos >= tadStart t && pos <= tadEnd t

-- | Check if a TAD is well-insulated (both boundaries strong)
isInsulated :: TAD -> Bool
isInsulated t = boundaryStrength (tadLeftBound t) >= 0.7
             && boundaryStrength (tadRightBound t) >= 0.7

-- | Model inter-TAD contact probability (should be low for well-insulated TADs)
interTADContact :: TAD -> TAD -> InteractionFreq
interTADContact t1 t2
  | tadChromosome t1 /= tadChromosome t2 = 0.01   -- Trans-chromosomal (very low)
  | otherwise =
      let dist = abs (tadStart t1 - tadStart t2)
          insulation = (boundaryStrength (tadRightBound t1)
                       + boundaryStrength (tadLeftBound t2)) / 2.0
      in (1.0 / fromIntegral (max 1 dist)) * (1.0 - insulation)

-- | Create a chromatin loop
mkLoop :: String -> LoopAnchor -> LoopAnchor -> Bool -> Double -> ChromatinLoop
mkLoop = ChromatinLoop

-- | Span of a loop in base pairs
loopSpan :: ChromatinLoop -> Int
loopSpan l = abs (anchorPosition (loopAnchorA l) - anchorPosition (loopAnchorB l))

-- | Check if a loop connects an enhancer to a promoter
isEnhancerPromoterLoop :: ChromatinLoop -> Bool
isEnhancerPromoterLoop l =
  (anchorType (loopAnchorA l) == EnhancerAnchor && anchorType (loopAnchorB l) == PromoterAnchor) ||
  (anchorType (loopAnchorA l) == PromoterAnchor && anchorType (loopAnchorB l) == EnhancerAnchor)

-- | Dereference a loop: resolve the enhancer-promoter contact.
--   Returns the pair (source name, target name) representing the
--   pointer dereference operation.
dereferenceLoop :: ChromatinLoop -> Maybe (String, String)
dereferenceLoop l
  | isEnhancerPromoterLoop l = Just (anchorName (loopAnchorA l), anchorName (loopAnchorB l))
  | otherwise                = Nothing

-- | Assign compartment based on simple GC content / gene density heuristic
assignCompartment :: GenomicRegion -> Compartment
assignCompartment r = case regionType r of
  EnhancerRegion  -> CompartmentA
  PromoterRegion  -> CompartmentA
  GeneBody        -> CompartmentA
  LADRegion       -> CompartmentB
  TelomereRegion  -> CompartmentB
  CentromereRegion -> CompartmentB
  InsulatorRegion -> CompartmentA
  TADRegion       -> CompartmentA
  IntergenicRegion -> CompartmentB

-- | Build compartment map for a list of regions
buildCompartmentMap :: [GenomicRegion] -> CompartmentMap
buildCompartmentMap rs = [(regionName r, assignCompartment r) | r <- rs]

-- | Check if a region is in compartment A
isCompartmentA :: CompartmentMap -> String -> Bool
isCompartmentA cmap name = lookup name cmap == Just CompartmentA

-- | Check if a region is in compartment B
isCompartmentB :: CompartmentMap -> String -> Bool
isCompartmentB cmap name = lookup name cmap == Just CompartmentB

-- | Nest TADs into a hierarchy based on containment
nestTADs :: [TAD] -> [SubTAD]
nestTADs [] = []
nestTADs tads =
  let sorted = sortBySize tads
  in map (\t -> SubTAD t (findChildren t sorted)) sorted
  where
    sortBySize = foldr insertBySize []
    insertBySize t [] = [t]
    insertBySize t (x:xs)
      | tadSize t >= tadSize x = t : x : xs
      | otherwise              = x : insertBySize t xs
    findChildren parent ts =
      [ SubTAD child [] | child <- ts
      , tadStart child >= tadStart parent
      , tadEnd child <= tadEnd parent
      , tadName child /= tadName parent
      ]

-- | Build a tree representation of TAD hierarchy
tadTree :: [SubTAD] -> String
tadTree = unlines . concatMap (showTree 0)
  where
    showTree indent (SubTAD t children) =
      (replicate indent ' ' ++ tadName t ++ " ["
       ++ show (tadStart t) ++ "-" ++ show (tadEnd t) ++ "]"
       ++ " (" ++ show (tadCompartment t) ++ ")")
      : concatMap (showTree (indent + 2)) children

-- | Build a simplified contact matrix for positions within TADs
buildContactMatrix :: [TAD] -> ContactMatrix
buildContactMatrix tads =
  [ ((tadStart t1, tadStart t2), freq)
  | t1 <- tads
  , t2 <- tads
  , let freq = if tadName t1 == tadName t2
               then 1.0  -- Intra-TAD: high contact
               else interTADContact t1 t2
  ]

-- | Look up contact frequency between two positions
lookupContact :: ContactMatrix -> (Int, Int) -> InteractionFreq
lookupContact cm pos = case lookup pos cm of
  Just f  -> f
  Nothing -> 0.0

-- | Convert contact matrix to displayable list
matrixToList :: ContactMatrix -> [((Int, Int), InteractionFreq)]
matrixToList = id

-- | Display TAD information
showTAD :: TAD -> String
showTAD t = unlines
  [ "TAD: " ++ tadName t
  , "  Location:    chr" ++ show (tadChromosome t) ++ ":"
    ++ show (tadStart t) ++ "-" ++ show (tadEnd t)
  , "  Size:        " ++ show (tadSize t) ++ " bp"
  , "  Compartment: " ++ show (tadCompartment t)
  , "  Insulated:   " ++ show (isInsulated t)
  , "  Loops:       " ++ show (length (tadLoops t))
  , "  Left bound:  " ++ show (boundaryProtein (tadLeftBound t))
    ++ " (strength " ++ show (boundaryStrength (tadLeftBound t)) ++ ")"
  , "  Right bound: " ++ show (boundaryProtein (tadRightBound t))
    ++ " (strength " ++ show (boundaryStrength (tadRightBound t)) ++ ")"
  ]

-- | Display a chromatin loop
showLoop :: ChromatinLoop -> String
showLoop l = loopName l ++ ": "
  ++ anchorName (loopAnchorA l) ++ " (" ++ show (anchorType (loopAnchorA l)) ++ ")"
  ++ " <-> "
  ++ anchorName (loopAnchorB l) ++ " (" ++ show (anchorType (loopAnchorB l)) ++ ")"
  ++ " [span=" ++ show (loopSpan l) ++ "bp"
  ++ ", cohesin=" ++ show (loopCohesin l)
  ++ ", strength=" ++ show (loopStrength l) ++ "]"

-- | Display compartment map
showCompartmentMap :: CompartmentMap -> String
showCompartmentMap cm = unlines $
  "=== Compartment Map ===" :
  [ "  " ++ name ++ " -> " ++ show comp | (name, comp) <- cm ]

-- | Display contact matrix summary
showContactMatrix :: ContactMatrix -> String
showContactMatrix cm = unlines $
  [ "=== Contact Matrix ==="
  , "Entries: " ++ show (length cm)
  ] ++
  [ "  (" ++ show r ++ ", " ++ show c ++ ") = " ++ show f
  | ((r, c), f) <- take 20 cm
  ]
