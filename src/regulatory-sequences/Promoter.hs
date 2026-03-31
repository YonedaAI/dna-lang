{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Promoter
-- Description : Promoter types, TATA box modeling, and binding affinity
-- Copyright   : (c) Matthew Long, YonedaAI Research Collective, 2026
-- License     : BSD-3-Clause
--
-- Promoter types modeled as function entry points in the DNA-Lang
-- regulatory control flow framework.

module Promoter
  ( Promoter(..)
  , PromoterStrength(..)
  , TATABox(..)
  , TranscriptionFactor(..)
  , TFContext
  , defaultPromoter
  , tataPromoter
  , tatalessPromoter
  , strongPromoter
  , computeBindingAffinity
  , promoterActivity
  , strengthMultiplier
  , hasTATABox
  , promoterInfo
  ) where

-- | Strength classification of a promoter
data PromoterStrength = Weak | Moderate | Strong | VeryStrong
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | TATA box representation
data TATABox = TATABox
  { tataSequence :: String    -- ^ Consensus is "TATAAA"
  , tataPosition :: Int       -- ^ Position relative to TSS (typically -25 to -30)
  , tataAffinity :: Double    -- ^ TBP binding affinity (0.0 to 1.0)
  } deriving (Show, Eq)

-- | A transcription factor that can bind to the promoter
data TranscriptionFactor = TranscriptionFactor
  { tfName       :: String
  , tfAffinity   :: Double    -- ^ Binding affinity to this promoter (0.0 to 1.0)
  , tfIsActivator :: Bool     -- ^ True for activator, False for repressor
  } deriving (Show, Eq)

-- | Transcription factor context: the set of TFs present in the cell
type TFContext = [TranscriptionFactor]

-- | Promoter type definition
data Promoter = Promoter
  { promoterSequence :: String
  , tataBox          :: Maybe TATABox
  , bindingAffinity  :: Double         -- ^ Overall binding affinity (0.0 to 1.0)
  , basalLevel       :: Double         -- ^ Basal transcription rate
  , promoterStrength :: PromoterStrength
  } deriving (Show, Eq)

-- | Default minimal promoter
defaultPromoter :: Promoter
defaultPromoter = Promoter
  { promoterSequence = "NNNNNNNNNNTATAAAAANNNNNNN"
  , tataBox          = Just (TATABox "TATAAA" (-25) 0.8)
  , bindingAffinity  = 0.5
  , basalLevel       = 0.1
  , promoterStrength = Moderate
  }

-- | A promoter with a strong TATA box
tataPromoter :: String -> Double -> Promoter
tataPromoter seq' affinity = Promoter
  { promoterSequence = seq'
  , tataBox          = Just (TATABox "TATAAA" (-25) affinity)
  , bindingAffinity  = affinity
  , basalLevel       = 0.1 * affinity
  , promoterStrength = if affinity > 0.8 then Strong
                       else if affinity > 0.5 then Moderate
                       else Weak
  }

-- | A TATA-less promoter (CpG island based)
tatalessPromoter :: String -> Double -> Promoter
tatalessPromoter seq' affinity = Promoter
  { promoterSequence = seq'
  , tataBox          = Nothing
  , bindingAffinity  = affinity * 0.7  -- slightly lower without TATA
  , basalLevel       = 0.05 * affinity
  , promoterStrength = if affinity > 0.9 then Moderate else Weak
  }

-- | A strong constitutive promoter (e.g., CMV)
strongPromoter :: Promoter
strongPromoter = Promoter
  { promoterSequence = "CMVPROMOTERSEQUENCE"
  , tataBox          = Just (TATABox "TATAAA" (-28) 0.95)
  , bindingAffinity  = 0.95
  , basalLevel       = 0.8
  , promoterStrength = VeryStrong
  }

-- | Compute the aggregate binding affinity for a promoter in a TF context
computeBindingAffinity :: Promoter -> TFContext -> Double
computeBindingAffinity p ctx =
  let activators = filter tfIsActivator ctx
      repressors = filter (not . tfIsActivator) ctx
      activationSum = sum [tfAffinity tf * bindingAffinity p | tf <- activators]
      repressionSum = sum [tfAffinity tf * bindingAffinity p | tf <- repressors]
      raw = basalLevel p + activationSum - repressionSum
  in max 0.0 (min 1.0 raw)

-- | Compute the promoter activity level given a TF context
-- Returns a value in [0.0, 1.0] representing the fraction of max expression
promoterActivity :: Promoter -> TFContext -> Double
promoterActivity p ctx =
  let aff = computeBindingAffinity p ctx
      mult = strengthMultiplier (promoterStrength p)
  in max 0.0 (min 1.0 (aff * mult))

-- | Convert promoter strength to a numeric multiplier
strengthMultiplier :: PromoterStrength -> Double
strengthMultiplier Weak       = 0.25
strengthMultiplier Moderate   = 0.5
strengthMultiplier Strong     = 0.75
strengthMultiplier VeryStrong = 1.0

-- | Check if a promoter has a TATA box
hasTATABox :: Promoter -> Bool
hasTATABox p = case tataBox p of
  Just _  -> True
  Nothing -> False

-- | Get a human-readable description of a promoter
promoterInfo :: Promoter -> String
promoterInfo p =
  "Promoter [" ++ show (promoterStrength p) ++
  ", basal=" ++ show (basalLevel p) ++
  ", affinity=" ++ show (bindingAffinity p) ++
  ", TATA=" ++ show (hasTATABox p) ++ "]"
