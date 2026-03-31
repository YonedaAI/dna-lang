{-
  MicroRNA.hs — miRNA seed matching, target prediction, and repression scoring

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Non-Coding RNAs as Signals and Middleware

  miRNAs are formalized as guard clauses: ~22nt sequences that
  post-transcriptionally repress targets via seed-region complementarity.
-}

module MicroRNA
  ( SeedMatch(..)
  , TargetPrediction(..)
  , seedMatchScore
  , findSeedMatches
  , predictTargets
  , repressionScore
  , guardEvaluate
  , cooperativeRepression
  , wcScore
  ) where

import RNAControl

-- | A seed match records where the seed binds and how strongly
data SeedMatch = SeedMatch
  { smPosition :: Int       -- ^ Position in target where match begins
  , smScore    :: Double    -- ^ Complementarity score [0,1]
  , smSeed     :: [Nucleotide]  -- ^ The seed sequence
  , smWindow   :: [Nucleotide]  -- ^ The matched window in target
  } deriving (Show, Eq)

-- | A target prediction with overall repression estimate
data TargetPrediction = TargetPrediction
  { tpTargetName :: String        -- ^ Name of target mRNA
  , tpTargetSeq  :: [Nucleotide]  -- ^ 3' UTR sequence
  , tpMatches    :: [SeedMatch]   -- ^ All seed match sites
  , tpRepression :: Double        -- ^ Predicted repression level [0,1]
  } deriving (Show, Eq)

-- | Watson-Crick complementarity score for a single pair.
--   Perfect complement = 1.0, wobble (G-U) = 0.5, mismatch = 0.0
wcScore :: Nucleotide -> Nucleotide -> Double
wcScore a b
  | isComplement a b = 1.0
  | isWobble a b     = 0.5
  | otherwise        = 0.0

-- | Score the complementarity between a seed and a target window.
--   The seed is compared against the reverse complement of the window
--   (since miRNAs bind antiparallel to their targets).
seedMatchScore :: [Nucleotide] -> [Nucleotide] -> Double
seedMatchScore seed window
  | length seed /= length window = 0.0
  | null seed                    = 0.0
  | otherwise =
      let rcWindow = reverseComplement window
          pairScores = zipWith wcScore seed rcWindow
          total = sum pairScores
      in total / fromIntegral (length seed)

-- | Slide the seed across a target sequence, returning all matches
--   above a minimum score threshold.
findSeedMatches :: [Nucleotide]  -- ^ Seed sequence
                -> [Nucleotide]  -- ^ Target sequence (3' UTR)
                -> Double        -- ^ Minimum score threshold
                -> [SeedMatch]
findSeedMatches seed target minScore =
  let k = length seed
      n = length target
      positions = [0 .. n - k]
      tryPosition i =
        let window = take k (drop i target)
            score  = seedMatchScore seed window
        in if score >= minScore
           then Just (SeedMatch i score seed window)
           else Nothing
  in concatMap (\i -> case tryPosition i of
                        Just m  -> [m]
                        Nothing -> []) positions

-- | Predict miRNA targets given a miRNA seed, a list of named target
--   3' UTR sequences, and a repression threshold.
predictTargets :: [Nucleotide]              -- ^ miRNA seed
               -> [(String, [Nucleotide])]  -- ^ (name, 3'UTR) pairs
               -> Double                    -- ^ Threshold
               -> [TargetPrediction]
predictTargets seed targets threshold =
  let predict (name, utr) =
        let matches = findSeedMatches seed utr threshold
            repLvl  = repressionScore matches
        in if not (null matches)
           then Just (TargetPrediction name utr matches repLvl)
           else Nothing
  in concatMap (\t -> case predict t of
                        Just p  -> [p]
                        Nothing -> []) targets

-- | Calculate overall repression score from a set of seed matches.
--   Multiple binding sites increase repression (cooperative effect).
repressionScore :: [SeedMatch] -> Double
repressionScore []      = 0.0
repressionScore matches =
  let baseScores = map smScore matches
      -- Each additional site contributes diminishingly
      weighted   = zipWith (\s i -> s * (0.8 ** fromIntegral i))
                           baseScores [0::Int ..]
      raw        = sum weighted
  in min 1.0 raw  -- Cap at 1.0

-- | Evaluate the miRNA guard clause.
--   Returns True if the guard fires (target should be repressed).
guardEvaluate :: RNAControl p -> [Nucleotide] -> Bool
guardEvaluate (MiRNA seed _ threshold) targetUTR =
  let matches = findSeedMatches seed targetUTR threshold
  in not (null matches)
guardEvaluate _ _ = False

-- | Model cooperative repression when two miRNAs target nearby sites.
--   Sites within 40nt of each other have enhanced repression.
cooperativeRepression :: [SeedMatch]  -- ^ Matches from miRNA 1
                      -> [SeedMatch]  -- ^ Matches from miRNA 2
                      -> Double       -- ^ Cooperativity factor alpha
                      -> Double       -- ^ Combined repression score
cooperativeRepression ms1 ms2 alpha =
  let base1 = repressionScore ms1
      base2 = repressionScore ms2
      -- Check for proximal sites (within 40nt)
      proximal = [ (m1, m2)
                 | m1 <- ms1, m2 <- ms2
                 , abs (smPosition m1 - smPosition m2) <= 40
                 ]
      boost = if null proximal then 0.0 else alpha
  in min 1.0 (base1 + base2 + boost)
