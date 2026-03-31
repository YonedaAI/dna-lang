{-
  SatelliteDNA.hs -- Tandem repeat modeling, centromeric and telomeric satellites

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Repetitive Elements as Self-Modifying Code
-}

module SatelliteDNA
  ( -- Satellite construction
    mkAlphaSatellite
  , mkTelomericRepeat
  , mkMinisatellite
  , mkMicrosatellite
  -- Tandem repeat operations
  , expandRepeat
  , contractRepeat
  , repeatArray
  -- Classification
  , satelliteClass
  , SatelliteClass(..)
  -- Analysis
  , repeatPurity
  , arrayLength
  , isAligned
  -- Telomere-specific
  , telomereLength
  , telomereShorten
  , telomereExtend
  -- Display
  , showSatellite
  , showRepeatArray
  ) where

import Repeat

-- | Classification of satellite DNA by repeat unit length.
data SatelliteClass
  = Microsatellite    -- 2-9 bp (STRs)
  | Minisatellite_    -- 10-100 bp (VNTRs)
  | Satellite_        -- 100-200+ bp (alpha-satellite, etc.)
  deriving (Show, Eq, Ord)

-- ─── Satellite Construction ─────────────────────────────────────────

-- | Create an alpha-satellite repeat (171 bp unit, centromeric).
mkAlphaSatellite :: Int -> Repeat
mkAlphaSatellite copies = SatelliteDNA
  { satRepeatUnit = alphaSatUnit
  , satCopyNumber = copies
  , satLocation   = Centromere
  }
  where
    -- Simplified 171 bp alpha-satellite consensus (first 20 bp shown, rest filled)
    alphaSatUnit = mkSequence (take 171 (cycle "AATCTCAAAGCGCTCCAAAG"
      ++ "TTTGTGTTTCGCTCAGTGACTTCCAATGAATGAAATCCCAACTCACAGAGTTGAACCTTCC"
      ++ "TTTTCATAGAGCAGTTTTGAAACTCTCTTTGTGATGTGTGCATTCAACTCACAGAGTTGAAC"
      ++ "CTTCCTTTTTGATGGAGCAGTTTTT"))

-- | Create a telomeric repeat (TTAGGG, vertebrate consensus).
mkTelomericRepeat :: Int -> Repeat
mkTelomericRepeat copies = SatelliteDNA
  { satRepeatUnit = mkSequence "TTAGGG"
  , satCopyNumber = copies
  , satLocation   = Telomere
  }

-- | Create a minisatellite with a custom repeat unit.
mkMinisatellite :: String -> Int -> SatelliteLocation -> Repeat
mkMinisatellite unitStr copies loc = SatelliteDNA
  { satRepeatUnit = mkSequence unitStr
  , satCopyNumber = copies
  , satLocation   = loc
  }

-- | Create a microsatellite (short tandem repeat).
mkMicrosatellite :: String -> Int -> Repeat
mkMicrosatellite unitStr copies = SatelliteDNA
  { satRepeatUnit = mkSequence unitStr
  , satCopyNumber = copies
  , satLocation   = Dispersed
  }

-- ─── Tandem Repeat Operations ───────────────────────────────────────

-- | Expand a satellite array by adding copies (replication slippage).
expandRepeat :: Int -> Repeat -> Repeat
expandRepeat n s@(SatelliteDNA _ copies _) =
  s { satCopyNumber = copies + n }
expandRepeat _ other = other

-- | Contract a satellite array by removing copies.
contractRepeat :: Int -> Repeat -> Repeat
contractRepeat n s@(SatelliteDNA _ copies _) =
  s { satCopyNumber = max 1 (copies - n) }
contractRepeat _ other = other

-- | Generate the full repeat array as a sequence.
repeatArray :: Repeat -> Sequence
repeatArray (SatelliteDNA unit n _) = concat (replicate n unit)
repeatArray _ = []

-- ─── Classification ─────────────────────────────────────────────────

-- | Classify a satellite by its repeat unit length.
satelliteClass :: Repeat -> Maybe SatelliteClass
satelliteClass (SatelliteDNA unit _ _)
  | len >= 2 && len <= 9   = Just Microsatellite
  | len >= 10 && len <= 100 = Just Minisatellite_
  | len > 100               = Just Satellite_
  | otherwise               = Nothing
  where len = length unit
satelliteClass _ = Nothing

-- ─── Analysis ───────────────────────────────────────────────────────

-- | Measure the purity of a repeat array (fraction of positions matching
--   the consensus unit). For a perfect array, this is 1.0.
--   This simplified version always returns 1.0 since we model ideal repeats.
repeatPurity :: Repeat -> Double
repeatPurity (SatelliteDNA _ _ _) = 1.0  -- perfect repeats in our model
repeatPurity _ = 0.0

-- | Total array length in base pairs.
arrayLength :: Repeat -> Int
arrayLength (SatelliteDNA unit n _) = length unit * n
arrayLength _ = 0

-- | Check if an array length is aligned to a boundary.
isAligned :: Int -> Repeat -> Bool
isAligned boundary sat = arrayLength sat `mod` boundary == 0

-- ─── Telomere-Specific Functions ────────────────────────────────────

-- | Get the length of a telomeric repeat array.
telomereLength :: Repeat -> Maybe Int
telomereLength (SatelliteDNA _ n Telomere) = Just (n * 6)  -- TTAGGG = 6 bp
telomereLength _ = Nothing

-- | Simulate telomere shortening (end-replication problem).
--   Each cell division loses approximately 50-200 bp (~8-33 repeats).
telomereShorten :: Int -> Repeat -> Repeat
telomereShorten repeatsLost s@(SatelliteDNA _ copies Telomere) =
  s { satCopyNumber = max 0 (copies - repeatsLost) }
telomereShorten _ other = other

-- | Simulate telomerase-mediated telomere extension.
telomereExtend :: Int -> Repeat -> Repeat
telomereExtend repeatsAdded s@(SatelliteDNA _ copies Telomere) =
  s { satCopyNumber = copies + repeatsAdded }
telomereExtend _ other = other

-- ─── Display ────────────────────────────────────────────────────────

-- | Show a satellite with classification info.
showSatellite :: Repeat -> String
showSatellite sat@(SatelliteDNA unit n loc) =
  let cls = case satelliteClass sat of
              Just Microsatellite  -> "Microsatellite (STR)"
              Just Minisatellite_  -> "Minisatellite (VNTR)"
              Just Satellite_      -> "Satellite"
              Nothing              -> "Unknown"
      unitStr = if length unit <= 20
                then showSequence unit
                else take 20 (showSequence unit) ++ "..."
  in cls ++ " at " ++ show loc ++ ": ("
     ++ unitStr ++ ")x" ++ show n
     ++ " = " ++ show (arrayLength sat) ++ " bp"
showSatellite other = repeatSummary other

-- | Show the repeat array (truncated for large arrays).
showRepeatArray :: Int -> Repeat -> String
showRepeatArray maxLen sat =
  let arr = showSequence (repeatArray sat)
      totalLen = arrayLength sat
  in if totalLen <= maxLen
     then arr
     else take maxLen arr ++ "... [" ++ show totalLen ++ " bp total]"
