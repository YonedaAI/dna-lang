{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Enhancer
-- Description : Enhancer types, distance effects, and chromatin looping model
-- Copyright   : (c) Matthew Long, YonedaAI Research Collective, 2026
-- License     : BSD-3-Clause
--
-- Enhancers modeled as remote configuration operators in the DNA-Lang
-- regulatory control flow framework. Implements the looping model for
-- action-at-a-distance effects.

module Enhancer
  ( Enhancer(..)
  , Insulator(..)
  , defaultEnhancer
  , distantEnhancer
  , superEnhancer
  , defaultInsulator
  , loopingProbability
  , effectiveEnhancement
  , composeEnhancers
  , effectiveEnhancementWithInsulator
  , enhancerInfo
  ) where

-- | Enhancer type definition
data Enhancer = Enhancer
  { enhancerSequence   :: String
  , targetDistance      :: Int          -- ^ Distance to target promoter in base pairs
  , foldChange         :: Double       -- ^ Fold-change multiplication factor (>= 1.0)
  , enhancerTFs        :: [String]     -- ^ Transcription factors that bind this enhancer
  , enhancerActivity   :: Double       -- ^ Activity level (0.0 to 1.0), tissue-dependent
  } deriving (Show, Eq)

-- | Insulator (boundary element) type
data Insulator = Insulator
  { insulatorSequence :: String
  , barrierStrength   :: Double        -- ^ Barrier strength (0.0 to 1.0)
  , ctcfBinding       :: Bool          -- ^ Whether CTCF protein binds
  } deriving (Show, Eq)

-- | Default enhancer at a moderate distance
defaultEnhancer :: Enhancer
defaultEnhancer = Enhancer
  { enhancerSequence = "ENHANCER_SEQ_DEFAULT"
  , targetDistance    = 10000        -- 10 kb
  , foldChange       = 5.0
  , enhancerTFs      = ["AP1", "SP1"]
  , enhancerActivity = 0.8
  }

-- | A distant enhancer (e.g., 500 kb away)
distantEnhancer :: Int -> Double -> Enhancer
distantEnhancer dist fc = Enhancer
  { enhancerSequence = "DISTANT_ENHANCER"
  , targetDistance    = dist
  , foldChange       = fc
  , enhancerTFs      = ["GATA1"]
  , enhancerActivity = 0.6
  }

-- | A super-enhancer (cluster of enhancers, very high fold-change)
superEnhancer :: Enhancer
superEnhancer = Enhancer
  { enhancerSequence = "SUPER_ENHANCER_CLUSTER"
  , targetDistance    = 5000
  , foldChange       = 50.0
  , enhancerTFs      = ["MYC", "OCT4", "SOX2", "NANOG"]
  , enhancerActivity = 0.95
  }

-- | Default CTCF insulator
defaultInsulator :: Insulator
defaultInsulator = Insulator
  { insulatorSequence = "CTCF_BINDING_SITE"
  , barrierStrength   = 0.9
  , ctcfBinding       = True
  }

-- | Compute looping probability as a function of genomic distance
-- Uses the power-law model: lambda(d) = lambda_0 * (d_0 / d)^gamma
-- Reference distance d_0 = 1000 bp, lambda_0 = 0.95, gamma = 1.2
loopingProbability :: Int -> Double
loopingProbability dist
  | dist <= 0   = 1.0
  | dist <= d0  = lambda0
  | otherwise   = lambda0 * (fromIntegral d0 / fromIntegral dist) ** gamma
  where
    d0     = 1000   :: Int
    lambda0 = 0.95  :: Double
    gamma   = 1.2   :: Double

-- | Compute effective enhancement of an enhancer on a promoter
-- E(E, P) = 1 + (mu - 1) * lambda(d) * activity
effectiveEnhancement :: Enhancer -> Double
effectiveEnhancement e =
  let mu     = foldChange e
      lp     = loopingProbability (targetDistance e)
      act    = enhancerActivity e
  in 1.0 + (mu - 1.0) * lp * act

-- | Compose multiple enhancers multiplicatively
-- When enhancers bind non-overlapping TFs, their effects multiply
composeEnhancers :: [Enhancer] -> Double
composeEnhancers [] = 1.0
composeEnhancers es = product (map effectiveEnhancement es)

-- | Effective enhancement with an insulator between enhancer and promoter
-- Attenuates looping probability by (1 - barrier_strength)
effectiveEnhancementWithInsulator :: Enhancer -> Insulator -> Double
effectiveEnhancementWithInsulator e ins =
  let mu     = foldChange e
      lp     = loopingProbability (targetDistance e)
      act    = enhancerActivity e
      att    = 1.0 - barrierStrength ins
  in 1.0 + (mu - 1.0) * lp * act * att

-- | Get a human-readable description of an enhancer
enhancerInfo :: Enhancer -> String
enhancerInfo e =
  "Enhancer [dist=" ++ show (targetDistance e) ++
  "bp, fold=" ++ show (foldChange e) ++
  "x, activity=" ++ show (enhancerActivity e) ++
  ", effective=" ++ show (effectiveEnhancement e) ++ "x]"
