{-
  Structure.hs — Core types for genome structural organization

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Structural DNA as Memory Architecture
-}

module Structure
  ( -- * Core genome layout type
    Structure(..)
  , Topology(..)
  , GenomicRegion(..)
  , RegionType(..)
  , AccessMap
  , AccessLevel(..)
  , Compartment(..)
    -- * Construction
  , emptyStructure
  , mkStructure
    -- * Queries
  , lookupAccess
  , regionsOfType
  , activeRegions
  , inactiveRegions
  , compartmentA
  , compartmentB
    -- * Modification
  , addRegion
  , setAccess
  , updateTopology
    -- * Memory hierarchy
  , MemoryTier(..)
  , memoryTier
  , ChromosomeTerritory(..)
  , mkTerritory
  , territoryContains
    -- * Display
  , showStructure
  , showRegion
  ) where

-- | Topology of the genome at various scales
data Topology
  = LinearTopology         -- ^ Linear chromosome (eukaryotic)
  | CircularTopology       -- ^ Circular chromosome (prokaryotic)
  | LoopedTopology Int     -- ^ Chromatin loop domain with loop count
  | HierarchicalTopology [Topology]  -- ^ Nested topological levels
  deriving (Show, Eq)

-- | Types of genomic regions in the structural model
data RegionType
  = TelomereRegion         -- ^ Chromosome end cap
  | CentromereRegion       -- ^ Chromosome segregation center
  | TADRegion              -- ^ Topologically associating domain
  | LADRegion              -- ^ Lamina-associated domain (peripheral)
  | EnhancerRegion         -- ^ Regulatory enhancer element
  | PromoterRegion         -- ^ Gene promoter
  | InsulatorRegion        -- ^ CTCF insulator boundary
  | GeneBody               -- ^ Transcribed gene region
  | IntergenicRegion       -- ^ Non-genic region
  deriving (Show, Eq, Ord)

-- | Chromatin compartment classification (Hi-C)
data Compartment
  = CompartmentA    -- ^ Active, euchromatic
  | CompartmentB    -- ^ Inactive, heterochromatic
  deriving (Show, Eq, Ord)

-- | Access level for a genomic region
data AccessLevel
  = Open             -- ^ Fully accessible (euchromatin)
  | Restricted       -- ^ Partially accessible (facultative heterochromatin)
  | Closed           -- ^ Inaccessible (constitutive heterochromatin)
  | Protected        -- ^ Protected from modification (imprinted)
  deriving (Show, Eq, Ord)

-- | A genomic region with position, type, and compartment
data GenomicRegion = GenomicRegion
  { regionName       :: String
  , regionType       :: RegionType
  , regionStart      :: Int          -- ^ Start position (bp)
  , regionEnd        :: Int          -- ^ End position (bp)
  , regionCompartment :: Compartment
  , regionChromosome :: Int          -- ^ Chromosome number
  } deriving (Show, Eq)

-- | Access map: list of (region name, access level) pairs
type AccessMap = [(String, AccessLevel)]

-- | Memory tier in the nuclear hierarchy
data MemoryTier
  = HotCache           -- ^ Transcription factories, active genes
  | WarmCache          -- ^ Poised genes, bivalent chromatin
  | ColdStorage        -- ^ Lamina-associated, deeply repressed
  deriving (Show, Eq, Ord)

-- | Chromosome territory in nuclear space
data ChromosomeTerritory = ChromosomeTerritory
  { territoryChrom    :: Int         -- ^ Chromosome number
  , territoryCenter   :: (Double, Double, Double)  -- ^ 3D center in nucleus
  , territoryRadius   :: Double      -- ^ Approximate radius
  , territoryRegions  :: [GenomicRegion]
  } deriving (Show, Eq)

-- | The core Structure type parameterized by genome layout metadata
data Structure g = Structure
  { topology       :: Topology
  , regions        :: [GenomicRegion]
  , accessibility  :: AccessMap
  , layoutMeta     :: g
  } deriving (Show, Eq)

-- | Empty structure with linear topology
emptyStructure :: Structure ()
emptyStructure = Structure
  { topology      = LinearTopology
  , regions       = []
  , accessibility = []
  , layoutMeta    = ()
  }

-- | Construct a Structure from components
mkStructure :: Topology -> [GenomicRegion] -> AccessMap -> g -> Structure g
mkStructure = Structure

-- | Look up the access level for a named region
lookupAccess :: String -> Structure g -> Maybe AccessLevel
lookupAccess name s = lookup name (accessibility s)

-- | Filter regions by type
regionsOfType :: RegionType -> Structure g -> [GenomicRegion]
regionsOfType rt s = filter (\r -> regionType r == rt) (regions s)

-- | Get all regions in compartment A (active)
activeRegions :: Structure g -> [GenomicRegion]
activeRegions = compartmentA

-- | Get all regions in compartment B (inactive)
inactiveRegions :: Structure g -> [GenomicRegion]
inactiveRegions = compartmentB

-- | Compartment A regions
compartmentA :: Structure g -> [GenomicRegion]
compartmentA s = filter (\r -> regionCompartment r == CompartmentA) (regions s)

-- | Compartment B regions
compartmentB :: Structure g -> [GenomicRegion]
compartmentB s = filter (\r -> regionCompartment r == CompartmentB) (regions s)

-- | Add a region to the structure
addRegion :: GenomicRegion -> Structure g -> Structure g
addRegion r s = s { regions = regions s ++ [r] }

-- | Set access level for a named region
setAccess :: String -> AccessLevel -> Structure g -> Structure g
setAccess name lvl s =
  s { accessibility = (name, lvl) : filter ((/= name) . fst) (accessibility s) }

-- | Update the topology
updateTopology :: Topology -> Structure g -> Structure g
updateTopology t s = s { topology = t }

-- | Determine the memory tier for a region based on compartment and access
memoryTier :: GenomicRegion -> AccessMap -> MemoryTier
memoryTier r amap =
  case lookup (regionName r) amap of
    Just Open       -> HotCache
    Just Restricted -> WarmCache
    Just Closed     -> ColdStorage
    Just Protected  -> ColdStorage
    Nothing         -> case regionCompartment r of
                         CompartmentA -> WarmCache
                         CompartmentB -> ColdStorage

-- | Create a chromosome territory
mkTerritory :: Int -> (Double, Double, Double) -> Double -> [GenomicRegion]
            -> ChromosomeTerritory
mkTerritory = ChromosomeTerritory

-- | Check if a region is within a territory
territoryContains :: ChromosomeTerritory -> GenomicRegion -> Bool
territoryContains ct r = regionChromosome r == territoryChrom ct

-- | Display a structure summary
showStructure :: Show g => Structure g -> String
showStructure s = unlines
  [ "=== Genome Structure ==="
  , "Topology: " ++ show (topology s)
  , "Regions:  " ++ show (length (regions s))
  , "  Active (A):   " ++ show (length (compartmentA s))
  , "  Inactive (B): " ++ show (length (compartmentB s))
  , "Access entries: " ++ show (length (accessibility s))
  , "Layout meta:    " ++ show (layoutMeta s)
  ]

-- | Display a single region
showRegion :: GenomicRegion -> String
showRegion r = regionName r ++ " [" ++ show (regionType r) ++ "] "
  ++ "chr" ++ show (regionChromosome r) ++ ":"
  ++ show (regionStart r) ++ "-" ++ show (regionEnd r)
  ++ " (" ++ show (regionCompartment r) ++ ")"
