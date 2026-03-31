{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Regulator
-- Description : Core Regulator type, expression level algebra, composition
-- Copyright   : (c) Matthew Long, YonedaAI Research Collective, 2026
-- License     : BSD-3-Clause
--
-- The core Regulator<a> parametric type that unifies promoters, enhancers,
-- and silencers with an expression level lattice.

module Regulator
  ( ExpressionLevel(..)
  , Silencer(..)
  , SilencerType(..)
  , Regulator(..)
  , latticeJoin
  , latticeMeet
  , levelToDouble
  , doubleToLevel
  , defaultSilencer
  , polycombSilencer
  , applySilencer
  , applyAllSilencers
  , composeRegulators
  , evaluateRegulator
  , regulatorInfo
  ) where

import Promoter (Promoter(..), promoterActivity, promoterInfo, TFContext)
import Enhancer (Enhancer(..), composeEnhancers, enhancerInfo)

-- | Expression level lattice: Off <= Basal <= Induced d <= Max
data ExpressionLevel
  = Off
  | Basal
  | Induced Double    -- ^ Parameterized induction level (0.0 < d < 1.0)
  | Max
  deriving (Show, Eq)

instance Ord ExpressionLevel where
  compare Off Off             = EQ
  compare Off _               = LT
  compare _ Off               = GT
  compare Max Max             = EQ
  compare Max _               = GT
  compare _ Max               = LT
  compare Basal Basal         = EQ
  compare Basal (Induced _)   = LT
  compare (Induced _) Basal   = GT
  compare (Induced a) (Induced b) = compare a b

-- | Lattice join (least upper bound)
latticeJoin :: ExpressionLevel -> ExpressionLevel -> ExpressionLevel
latticeJoin Off x               = x
latticeJoin x Off               = x
latticeJoin Max _               = Max
latticeJoin _ Max               = Max
latticeJoin Basal Basal         = Basal
latticeJoin Basal (Induced d)   = Induced d
latticeJoin (Induced d) Basal   = Induced d
latticeJoin (Induced a) (Induced b) = Induced (max a b)

-- | Lattice meet (greatest lower bound)
latticeMeet :: ExpressionLevel -> ExpressionLevel -> ExpressionLevel
latticeMeet Off _               = Off
latticeMeet _ Off               = Off
latticeMeet Max x               = x
latticeMeet x Max               = x
latticeMeet Basal Basal         = Basal
latticeMeet Basal (Induced _)   = Basal
latticeMeet (Induced _) Basal   = Basal
latticeMeet (Induced a) (Induced b) = Induced (min a b)

-- | Convert an expression level to a numeric value in [0, 1]
levelToDouble :: ExpressionLevel -> Double
levelToDouble Off          = 0.0
levelToDouble Basal        = 0.1
levelToDouble (Induced d)  = 0.1 + 0.8 * (max 0.0 (min 1.0 d))
levelToDouble Max          = 1.0

-- | Convert a numeric value in [0, 1] to an expression level
doubleToLevel :: Double -> ExpressionLevel
doubleToLevel x
  | x <= 0.01  = Off
  | x <= 0.15  = Basal
  | x >= 0.95  = Max
  | otherwise   = Induced ((x - 0.1) / 0.8)

-- | Silencer type classification
data SilencerType = Classical | Polycomb | Heterochromatin
  deriving (Show, Eq)

-- | Silencer type definition
data Silencer = Silencer
  { silencerSequence :: String
  , repressionLevel  :: Double        -- ^ 0.0 = no effect, 1.0 = full silencing
  , silencerType     :: SilencerType
  } deriving (Show, Eq)

-- | Default classical silencer
defaultSilencer :: Silencer
defaultSilencer = Silencer
  { silencerSequence = "SILENCER_CLASSICAL"
  , repressionLevel  = 0.7
  , silencerType     = Classical
  }

-- | Polycomb-mediated silencer (epigenetic repression)
polycombSilencer :: Silencer
polycombSilencer = Silencer
  { silencerSequence = "PRC2_BINDING_SITE"
  , repressionLevel  = 0.9
  , silencerType     = Polycomb
  }

-- | Apply a single silencer to an expression level
applySilencer :: Silencer -> Double -> Double
applySilencer s level = level * (1.0 - repressionLevel s)

-- | Apply all silencers (uses maximum repression model)
applyAllSilencers :: [Silencer] -> Double -> Double
applyAllSilencers [] level = level
applyAllSilencers ss level =
  let maxRepression = maximum (map repressionLevel ss)
  in level * (1.0 - maxRepression)

-- | The core Regulator type, parametric in input signal type a
data Regulator a = Regulator
  { regPromoter   :: Promoter
  , regEnhancers  :: [Enhancer]
  , regSilencers  :: [Silencer]
  , regOutput     :: a -> ExpressionLevel
  }

-- | Compose two regulators: takes promoter from first, merges enhancers
-- and silencers, output is the meet of both outputs
composeRegulators :: Regulator a -> Regulator b -> Regulator (a, b)
composeRegulators r1 r2 = Regulator
  { regPromoter  = regPromoter r1
  , regEnhancers = regEnhancers r1 ++ regEnhancers r2
  , regSilencers = regSilencers r1 ++ regSilencers r2
  , regOutput    = \(a, b) ->
      latticeMeet (regOutput r1 a) (regOutput r2 b)
  }

-- | Evaluate a regulator with a TF context and input signal
-- Combines: promoter activity * enhancer boost * silencer damping
evaluateRegulator :: Regulator a -> TFContext -> a -> ExpressionLevel
evaluateRegulator reg ctx input =
  let promActivity = promoterActivity (regPromoter reg) ctx
      enhBoost     = composeEnhancers (regEnhancers reg)
      boosted      = promActivity * enhBoost
      silenced     = applyAllSilencers (regSilencers reg) boosted
      fromOutput   = levelToDouble (regOutput reg input)
      combined     = min 1.0 (silenced * fromOutput / (max 0.01 fromOutput + 0.01))
  in if null (regSilencers reg) && null (regEnhancers reg)
     then regOutput reg input
     else doubleToLevel (min 1.0 (silenced * (levelToDouble (regOutput reg input))))

-- | Get a human-readable description of a regulator
regulatorInfo :: Regulator a -> String
regulatorInfo reg =
  "Regulator {\n" ++
  "  promoter: " ++ promoterInfo (regPromoter reg) ++ "\n" ++
  "  enhancers: " ++ show (length (regEnhancers reg)) ++ " [" ++
    concatMap (\e -> enhancerInfo e ++ "; ") (regEnhancers reg) ++ "]\n" ++
  "  silencers: " ++ show (length (regSilencers reg)) ++ " [" ++
    concatMap (\s -> show (repressionLevel s) ++ " ") (regSilencers reg) ++ "]\n" ++
  "}"
