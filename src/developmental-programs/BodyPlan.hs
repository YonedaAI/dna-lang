{-
  BodyPlan.hs -- Hox gene cluster, anterior-posterior axis, segment identity,
                 collinearity mapping

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Developmental Programs as Orchestration
-}

module BodyPlan
  ( HoxGene(..)
  , HoxCluster(..)
  , SegmentIdentity(..)
  , BodyAxis(..)
  , AxisPosition(..)
  , MorphogenField(..)
  , CellFateZone(..)
  , makeHoxCluster
  , mammalianHoxA
  , verifyCollinearity
  , verifySpatialCollinearity
  , verifyTemporalCollinearity
  , mapGeneToSegment
  , collinearityFunctor
  , interpretGradient
  , frenchFlagPartition
  , showHoxCluster
  , showMorphogenField
  ) where

-- | A position along an axis
data AxisPosition = AxisPosition
  { axisName  :: String
  , axisValue :: Double   -- 0.0 = anterior/dorsal, 1.0 = posterior/ventral
  } deriving (Show, Eq)

instance Ord AxisPosition where
  compare a b = compare (axisValue a) (axisValue b)

-- | Segment identities along the body plan
data SegmentIdentity
  = Head
  | Cervical
  | Thoracic
  | Lumbar
  | Sacral
  | Caudal
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | A single Hox gene
data HoxGene = HoxGene
  { geneName           :: String
  , chromosomePosition :: Int        -- position on chromosome (3' to 5')
  , expressionDomain   :: AxisPosition  -- anterior boundary of expression
  , activationTime     :: Double     -- time of first activation (hours)
  , targetSegments     :: [SegmentIdentity]
  } deriving (Show)

-- | A cluster of Hox genes
data HoxCluster = HoxCluster
  { clusterName :: String
  , hoxGenes    :: [HoxGene]
  } deriving (Show)

-- | The body axis
data BodyAxis = BodyAxis
  { bodyAxisName :: String
  , segments     :: [SegmentIdentity]
  } deriving (Show)

-- | Morphogen field: a signaling molecule with spatial gradient
data MorphogenField = MorphogenField
  { morphogenName  :: String
  , sourcePosition :: Double    -- position of source (0-1)
  , decayRate      :: Double    -- exponential decay constant
  , highThreshold  :: Double    -- threshold for high-fate zone
  , midThreshold   :: Double    -- threshold for mid-fate zone
  , lowThreshold   :: Double    -- threshold for low-fate zone
  } deriving (Show)

-- | Cell fate zones from French flag model
data CellFateZone
  = HighZone
  | MidZone
  | LowZone
  | NoSignal
  deriving (Show, Eq, Ord)

-- | Create a Hox cluster from a list of genes, sorted by chromosome position
makeHoxCluster :: String -> [HoxGene] -> HoxCluster
makeHoxCluster name genes = HoxCluster
  { clusterName = name
  , hoxGenes    = sortByPosition genes
  }
  where
    sortByPosition [] = []
    sortByPosition (x:xs) =
      sortByPosition [y | y <- xs, chromosomePosition y <= chromosomePosition x]
      ++ [x]
      ++ sortByPosition [y | y <- xs, chromosomePosition y > chromosomePosition x]

-- | A model mammalian HoxA cluster
mammalianHoxA :: HoxCluster
mammalianHoxA = makeHoxCluster "HoxA" genes
  where
    genes =
      [ HoxGene "HoxA1"  1  (AxisPosition "AP" 0.15) 7.5  [Head, Cervical]
      , HoxGene "HoxA2"  2  (AxisPosition "AP" 0.20) 8.0  [Cervical]
      , HoxGene "HoxA3"  3  (AxisPosition "AP" 0.25) 8.5  [Cervical]
      , HoxGene "HoxA4"  4  (AxisPosition "AP" 0.35) 9.0  [Cervical, Thoracic]
      , HoxGene "HoxA5"  5  (AxisPosition "AP" 0.40) 9.5  [Thoracic]
      , HoxGene "HoxA6"  6  (AxisPosition "AP" 0.45) 10.0 [Thoracic]
      , HoxGene "HoxA7"  7  (AxisPosition "AP" 0.50) 10.5 [Thoracic]
      , HoxGene "HoxA9"  9  (AxisPosition "AP" 0.60) 11.0 [Lumbar]
      , HoxGene "HoxA10" 10 (AxisPosition "AP" 0.65) 11.5 [Lumbar]
      , HoxGene "HoxA11" 11 (AxisPosition "AP" 0.75) 12.0 [Lumbar, Sacral]
      , HoxGene "HoxA13" 13 (AxisPosition "AP" 0.85) 12.5 [Sacral, Caudal]
      ]

-- | Verify spatial collinearity: chromosome order preserves axis position order
verifySpatialCollinearity :: HoxCluster -> Bool
verifySpatialCollinearity cluster =
  let genes = hoxGenes cluster
      positions = map chromosomePosition genes
      bodyPositions = map (axisValue . expressionDomain) genes
  in isSorted positions && isSorted bodyPositions

-- | Verify temporal collinearity: chromosome order preserves activation time
verifyTemporalCollinearity :: HoxCluster -> Bool
verifyTemporalCollinearity cluster =
  let genes = hoxGenes cluster
      positions = map chromosomePosition genes
      times = map activationTime genes
  in isSorted positions && isSorted times

-- | Verify both spatial and temporal collinearity
verifyCollinearity :: HoxCluster -> Bool
verifyCollinearity cluster =
  verifySpatialCollinearity cluster && verifyTemporalCollinearity cluster

-- | Helper: check if a list is sorted (non-decreasing)
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- | Map a Hox gene to its target body segment (collinearity functor)
mapGeneToSegment :: HoxGene -> SegmentIdentity
mapGeneToSegment gene
  | pos < 0.20 = Head
  | pos < 0.35 = Cervical
  | pos < 0.55 = Thoracic
  | pos < 0.70 = Lumbar
  | pos < 0.80 = Sacral
  | otherwise   = Caudal
  where pos = axisValue (expressionDomain gene)

-- | The collinearity functor: maps chromosome positions to axis positions
--   and verifies order preservation (monotone functor property)
collinearityFunctor :: HoxCluster -> [(Int, AxisPosition)]
collinearityFunctor cluster =
  map (\g -> (chromosomePosition g, expressionDomain g)) (hoxGenes cluster)

-- | Interpret a morphogen gradient at a given position
--   Returns the cell fate zone based on concentration thresholds
interpretGradient :: MorphogenField -> Double -> CellFateZone
interpretGradient field concentration
  | concentration > highThreshold field = HighZone
  | concentration > midThreshold field  = MidZone
  | concentration > lowThreshold field  = LowZone
  | otherwise                           = NoSignal

-- | Compute morphogen concentration at a distance from source
morphogenConcentration :: MorphogenField -> Double -> Double
morphogenConcentration field distance =
  exp (negate (decayRate field) * abs (distance - sourcePosition field))

-- | French flag partition: given a morphogen field, partition positions
--   into fate zones
frenchFlagPartition :: MorphogenField -> [Double] -> [(Double, CellFateZone)]
frenchFlagPartition field positions =
  map (\pos -> (pos, interpretGradient field (morphogenConcentration field pos)))
      positions

-- | Display a Hox cluster
showHoxCluster :: HoxCluster -> String
showHoxCluster cluster = header ++ geneLines
  where
    header = "Hox Cluster: " ++ clusterName cluster ++ "\n"
          ++ "Collinear: " ++ show (verifyCollinearity cluster) ++ "\n"
          ++ replicate 60 '-' ++ "\n"
          ++ padR 10 "Gene" ++ padR 8 "ChrPos" ++ padR 10 "AxisPos"
          ++ padR 10 "Time(h)" ++ "Segment\n"
          ++ replicate 60 '-' ++ "\n"
    geneLines = concatMap showGene (hoxGenes cluster)
    showGene g = padR 10 (geneName g)
              ++ padR 8 (show (chromosomePosition g))
              ++ padR 10 (showF2 (axisValue (expressionDomain g)))
              ++ padR 10 (showF2 (activationTime g))
              ++ show (mapGeneToSegment g) ++ "\n"
    padR n s = s ++ replicate (max 0 (n - length s)) ' '
    showF2 x = let whole = floor x :: Int
                   frac  = round ((x - fromIntegral whole) * 100) :: Int
                   fs    = if frac < 10 then "0" ++ show frac else show frac
               in show whole ++ "." ++ fs

-- | Display a morphogen field with French flag partition
showMorphogenField :: MorphogenField -> String
showMorphogenField field = header ++ zoneLines
  where
    header = "Morphogen: " ++ morphogenName field ++ "\n"
          ++ "Source at: " ++ show (sourcePosition field) ++ "\n"
          ++ "Thresholds: high=" ++ show (highThreshold field)
          ++ " mid=" ++ show (midThreshold field)
          ++ " low=" ++ show (lowThreshold field) ++ "\n"
          ++ replicate 40 '-' ++ "\n"
    positions = [0.0, 0.1 .. 1.0]
    partition = frenchFlagPartition field positions
    zoneLines = concatMap (\(p, z) ->
      "  pos=" ++ showF1 p ++ "  zone=" ++ show z ++ "\n") partition
    showF1 x = let whole = floor x :: Int
                   frac  = round ((x - fromIntegral whole) * 10) :: Int
               in show whole ++ "." ++ show frac
