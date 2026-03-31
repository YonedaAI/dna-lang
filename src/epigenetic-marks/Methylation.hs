{-
  Methylation.hs -- CpG methylation model with DNMT/TET enzymes

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Epigenetic Marks as Runtime State

  This module models DNA methylation as configuration flags on genomic
  regions. CpG dinucleotides can be methylated (5mC) or unmethylated,
  with DNMT enzymes as writers and TET enzymes as erasers.

  The methylation state forms a monad on gene expression:
    return = unmethylated (default permissive state)
    bind   = methylation-dependent expression gating
-}

module Methylation
  ( MethylationStatus(..)
  , MethylationMap(..)
  , CpGSite(..)
  , CpGIsland(..)
  , EnzymeType(..)
  , MethylationMonad(..)
  , emptyMethylationMap
  , combineMethylationMaps
  , showMethylationMap
  , methylateSite
  , demethylateSite
  , getSiteStatus
  , countMethylated
  , countUnmethylated
  , methylationFraction
  , isCpGIsland
  , detectCpGIslands
  , applyDNMT3
  , applyDNMT1
  , applyTET
  , maintenanceMethylation
  , showMethylationStatus
  , runMethylation
  ) where

-- | Status of a single CpG site
data MethylationStatus
  = Unmethylated       -- ^ No methylation (cytosine)
  | Hemimethylated     -- ^ One strand methylated (post-replication)
  | FullyMethylated    -- ^ Both strands methylated (5mC)
  | Hydroxymethylated  -- ^ TET-oxidized (5hmC, intermediate)
  deriving (Eq, Ord, Show, Read)

-- | A CpG dinucleotide site at a genomic position
data CpGSite = CpGSite
  { sitePosition :: Int
  , siteStatus   :: MethylationStatus
  } deriving (Eq, Show, Read)

-- | A CpG island: cluster of CpG sites
data CpGIsland = CpGIsland
  { islandStart    :: Int
  , islandEnd      :: Int
  , islandSites    :: [CpGSite]
  , islandDensity  :: Double  -- ^ CpG observed/expected ratio
  } deriving (Eq, Show, Read)

-- | Map of methylation states across a region
data MethylationMap = MethylationMap
  { methylationSites :: [CpGSite]
  } deriving (Eq, Show, Read)

-- | Enzyme types that modify methylation
data EnzymeType
  = DNMT1    -- ^ Maintenance methyltransferase (copies methylation after replication)
  | DNMT3A   -- ^ De novo methyltransferase A
  | DNMT3B   -- ^ De novo methyltransferase B
  | TET1     -- ^ Ten-eleven translocation enzyme 1 (oxidizes 5mC to 5hmC)
  | TET2     -- ^ Ten-eleven translocation enzyme 2
  | TET3     -- ^ Ten-eleven translocation enzyme 3
  deriving (Eq, Show, Read)

-- | Methylation monad: wraps a value with methylation context
data MethylationMonad a = MethylationMonad
  { methylationContext :: MethylationMap
  , methylationValue   :: a
  } deriving (Eq, Show, Read)

-- | Functor instance for MethylationMonad
instance Functor MethylationMonad where
  fmap f (MethylationMonad ctx v) = MethylationMonad ctx (f v)

-- | Applicative instance for MethylationMonad
instance Applicative MethylationMonad where
  pure a = MethylationMonad emptyMethylationMap a
  (MethylationMonad ctx1 f) <*> (MethylationMonad ctx2 v) =
    MethylationMonad (combineMethylationMaps ctx1 ctx2) (f v)

-- | Monad instance: bind gates expression based on methylation
instance Monad MethylationMonad where
  return = pure
  (MethylationMonad ctx v) >>= f =
    let (MethylationMonad ctx2 v2) = f v
    in MethylationMonad (combineMethylationMaps ctx ctx2) v2

-- | Run the methylation monad, extracting value and final state
runMethylation :: MethylationMonad a -> (MethylationMap, a)
runMethylation (MethylationMonad ctx v) = (ctx, v)

-- | Empty methylation map (no CpG sites tracked)
emptyMethylationMap :: MethylationMap
emptyMethylationMap = MethylationMap []

-- | Combine two methylation maps (union of sites, prefer first on conflict)
combineMethylationMaps :: MethylationMap -> MethylationMap -> MethylationMap
combineMethylationMaps (MethylationMap s1) (MethylationMap s2) =
  MethylationMap (mergeSites s1 s2)
  where
    mergeSites [] ys = ys
    mergeSites xs [] = xs
    mergeSites (x:xs) ys =
      let ys' = filter (\y -> sitePosition y /= sitePosition x) ys
      in x : mergeSites xs ys'

-- | Pretty-print a methylation map
showMethylationMap :: MethylationMap -> String
showMethylationMap (MethylationMap []) = "(no CpG sites)"
showMethylationMap (MethylationMap sites) =
  show (length sites) ++ " CpG sites ["
  ++ show (countMethylatedList sites) ++ " methylated, "
  ++ show (countUnmethylatedList sites) ++ " unmethylated]"

-- | Methylate a site at given position (de novo: DNMT3)
methylateSite :: Int -> MethylationMap -> MethylationMap
methylateSite pos (MethylationMap sites) =
  MethylationMap (updateOrAdd pos FullyMethylated sites)

-- | Demethylate a site at given position (TET-mediated)
demethylateSite :: Int -> MethylationMap -> MethylationMap
demethylateSite pos (MethylationMap sites) =
  MethylationMap (updateOrAdd pos Unmethylated sites)

-- | Get status of a site at given position
getSiteStatus :: Int -> MethylationMap -> MethylationStatus
getSiteStatus pos (MethylationMap sites) =
  case filter (\s -> sitePosition s == pos) sites of
    (s:_) -> siteStatus s
    []    -> Unmethylated  -- default: unmethylated

-- | Count methylated sites
countMethylated :: MethylationMap -> Int
countMethylated (MethylationMap sites) = countMethylatedList sites

countMethylatedList :: [CpGSite] -> Int
countMethylatedList = length . filter (\s -> siteStatus s == FullyMethylated)

-- | Count unmethylated sites
countUnmethylated :: MethylationMap -> Int
countUnmethylated (MethylationMap sites) = countUnmethylatedList sites

countUnmethylatedList :: [CpGSite] -> Int
countUnmethylatedList = length . filter (\s -> siteStatus s == Unmethylated)

-- | Fraction of sites that are methylated
methylationFraction :: MethylationMap -> Double
methylationFraction (MethylationMap []) = 0.0
methylationFraction (MethylationMap sites) =
  fromIntegral (countMethylatedList sites) / fromIntegral (length sites)

-- | Check if a region qualifies as a CpG island
--   Criteria: >200bp, CpG observed/expected > 0.6, GC content > 50%
isCpGIsland :: Int -> Int -> [CpGSite] -> Bool
isCpGIsland start end sites =
  let regionLen = end - start
      siteCount = length (filter (\s -> sitePosition s >= start
                                     && sitePosition s <= end) sites)
      density   = if regionLen > 0
                  then fromIntegral siteCount / fromIntegral regionLen * 100.0
                  else 0.0 :: Double
  in regionLen >= 200 && density > 1.0

-- | Detect CpG islands in a list of sites using sliding window
detectCpGIslands :: [CpGSite] -> [CpGIsland]
detectCpGIslands [] = []
detectCpGIslands sites =
  let sorted     = sortSites sites
      clusters   = clusterSites 200 sorted
  in [ CpGIsland (sitePosition (head c)) (sitePosition (last c)) c
       (fromIntegral (length c) / fromIntegral (sitePosition (last c) - sitePosition (head c) + 1) * 100.0)
     | c <- clusters
     , length c >= 5  -- minimum 5 CpG sites for an island
     ]

-- | Sort sites by position
sortSites :: [CpGSite] -> [CpGSite]
sortSites [] = []
sortSites (x:xs) =
  sortSites [y | y <- xs, sitePosition y <= sitePosition x]
  ++ [x]
  ++ sortSites [y | y <- xs, sitePosition y > sitePosition x]

-- | Cluster nearby CpG sites (within maxGap bp)
clusterSites :: Int -> [CpGSite] -> [[CpGSite]]
clusterSites _ [] = []
clusterSites maxGap (s:ss) = go [s] ss
  where
    go acc [] = [reverse acc]
    go acc (x:xs)
      | sitePosition x - sitePosition (head acc) <= maxGap = go (x:acc) xs
      | otherwise = reverse acc : go [x] xs

-- | Apply DNMT3 (de novo methyltransferase): methylate unmethylated sites
applyDNMT3 :: [Int] -> MethylationMap -> MethylationMap
applyDNMT3 positions mm = foldl (flip methylateSite) mm positions

-- | Apply DNMT1 (maintenance methyltransferase): restore hemimethylated to fully methylated
applyDNMT1 :: MethylationMap -> MethylationMap
applyDNMT1 (MethylationMap sites) =
  MethylationMap (map maintainSite sites)
  where
    maintainSite s
      | siteStatus s == Hemimethylated = s { siteStatus = FullyMethylated }
      | otherwise                      = s

-- | Apply TET enzyme: oxidize 5mC to 5hmC (first step of active demethylation)
applyTET :: [Int] -> MethylationMap -> MethylationMap
applyTET positions (MethylationMap sites) =
  MethylationMap (map oxidize sites)
  where
    oxidize s
      | sitePosition s `elem` positions && siteStatus s == FullyMethylated
        = s { siteStatus = Hydroxymethylated }
      | otherwise = s

-- | Maintenance methylation across cell division:
--   FullyMethylated -> Hemimethylated (replication)
--   then DNMT1 restores to FullyMethylated
maintenanceMethylation :: MethylationMap -> MethylationMap
maintenanceMethylation = applyDNMT1 . replicationDilution

-- | Simulate replication: fully methylated becomes hemimethylated
replicationDilution :: MethylationMap -> MethylationMap
replicationDilution (MethylationMap sites) =
  MethylationMap (map dilute sites)
  where
    dilute s
      | siteStatus s == FullyMethylated = s { siteStatus = Hemimethylated }
      | otherwise                       = s

-- | Pretty-print methylation status
showMethylationStatus :: MethylationStatus -> String
showMethylationStatus Unmethylated      = "C  (unmethylated)"
showMethylationStatus Hemimethylated    = "hmC (hemimethylated)"
showMethylationStatus FullyMethylated   = "5mC (methylated)"
showMethylationStatus Hydroxymethylated = "5hmC (hydroxymethylated)"

-- Helper: update a site or add new one
updateOrAdd :: Int -> MethylationStatus -> [CpGSite] -> [CpGSite]
updateOrAdd pos status [] = [CpGSite pos status]
updateOrAdd pos status (s:ss)
  | sitePosition s == pos = s { siteStatus = status } : ss
  | otherwise             = s : updateOrAdd pos status ss

-- | Pretty-print a methylation status for display
showMethylationStatusShort :: MethylationStatus -> String
showMethylationStatusShort Unmethylated      = "U"
showMethylationStatusShort Hemimethylated    = "H"
showMethylationStatusShort FullyMethylated   = "M"
showMethylationStatusShort Hydroxymethylated = "O"
