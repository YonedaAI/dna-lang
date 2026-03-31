{-
  Telomere.hs — Linear resource types modeling telomere biology

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Structural DNA as Memory Architecture

  Telomeres are formalized as linear types: each cell division consumes
  a fixed quantum of telomere repeats, and the resource cannot be
  duplicated without the telomerase enzyme. When the resource is
  exhausted, the cell enters replicative senescence (Hayflick limit).
-}

module Telomere
  ( -- * Core types
    Telomere(..)
  , TelomereState(..)
  , TelomeraseActivity(..)
    -- * Constants
  , ttagggRepeat
  , initialRepeatCount
  , criticalLength
  , hayflickLimit
  , shorteningPerDivision
    -- * Construction
  , newTelomere
  , newTelomereWithLength
    -- * Linear resource operations
  , divide
  , divideN
  , telomeraseExtend
    -- * Queries
  , telomereState
  , remainingDivisions
  , repeatCount
  , basePairLength
  , isCritical
  , isSenescent
    -- * Simulation
  , simulateLifespan
  , simulateWithTelomerase
    -- * Display
  , showTelomere
  , showLifespan
  ) where

-- | The canonical human telomere repeat sequence
ttagggRepeat :: String
ttagggRepeat = "TTAGGG"

-- | Typical initial number of TTAGGG repeats (~2500 for 15kb)
initialRepeatCount :: Int
initialRepeatCount = 2500

-- | Critical repeat count below which senescence is triggered (~800 for ~5kb)
criticalLength :: Int
criticalLength = 800

-- | Classical Hayflick limit for human fibroblasts
hayflickLimit :: Int
hayflickLimit = 50

-- | Repeats lost per cell division (~50bp = ~8 repeats of 6bp)
shorteningPerDivision :: Int
shorteningPerDivision = 8

-- | State of a telomere resource
data TelomereState
  = Healthy          -- ^ Sufficient length, protected by shelterin
  | Shortened        -- ^ Below optimal but above critical
  | Critical         -- ^ Near Hayflick limit, DNA damage response activated
  | Senescent        -- ^ Exhausted, cell cycle arrest
  | Crisis           -- ^ Below senescence, genomic instability
  deriving (Show, Eq, Ord)

-- | Telomerase activity level
data TelomeraseActivity
  = NoTelomerase       -- ^ Somatic cells: no extension
  | LowTelomerase      -- ^ Stem cells: partial maintenance
  | HighTelomerase     -- ^ Germ cells / cancer: full maintenance
  deriving (Show, Eq, Ord)

-- | A telomere as a linear resource
--   The key invariant: repeats can only decrease (without telomerase)
--   and cannot be duplicated.
data Telomere = Telomere
  { repeats          :: Int              -- ^ Current TTAGGG repeat count
  , divisionsElapsed :: Int              -- ^ Number of divisions undergone
  , telomerase       :: TelomeraseActivity  -- ^ Telomerase status
  , shelterinIntact  :: Bool             -- ^ Whether shelterin complex is bound
  } deriving (Show, Eq)

-- | Create a new telomere with default initial length
newTelomere :: Telomere
newTelomere = Telomere
  { repeats          = initialRepeatCount
  , divisionsElapsed = 0
  , telomerase       = NoTelomerase
  , shelterinIntact  = True
  }

-- | Create a telomere with a specific repeat count
newTelomereWithLength :: Int -> TelomeraseActivity -> Telomere
newTelomereWithLength n ta = Telomere
  { repeats          = max 0 n
  , divisionsElapsed = 0
  , telomerase       = ta
  , shelterinIntact  = n > criticalLength
  }

-- | Consume one division worth of telomere (linear type consumption).
--   Returns Nothing if the telomere is already senescent (resource exhausted).
divide :: Telomere -> Maybe Telomere
divide t
  | repeats t <= 0 = Nothing   -- Resource fully consumed
  | otherwise =
      let lost = case telomerase t of
                   NoTelomerase   -> shorteningPerDivision
                   LowTelomerase  -> shorteningPerDivision `div` 2
                   HighTelomerase -> 0  -- Telomerase maintains length
          newRepeats = max 0 (repeats t - lost)
          newDiv     = divisionsElapsed t + 1
          shelterin  = newRepeats > criticalLength
      in Just $ t { repeats         = newRepeats
                  , divisionsElapsed = newDiv
                  , shelterinIntact = shelterin
                  }

-- | Perform n divisions, short-circuiting on senescence
divideN :: Int -> Telomere -> (Telomere, Int)
divideN 0 t = (t, 0)
divideN n t = case divide t of
  Nothing -> (t, 0)
  Just t' -> let (final, completed) = divideN (n - 1) t'
             in (final, completed + 1)

-- | Extend telomere via telomerase (only if telomerase is active)
telomeraseExtend :: Int -> Telomere -> Telomere
telomeraseExtend amount t = case telomerase t of
  NoTelomerase  -> t  -- Cannot extend without telomerase
  LowTelomerase -> t { repeats = repeats t + (amount `div` 2) }
  HighTelomerase -> t { repeats = repeats t + amount }

-- | Determine the current state of the telomere
telomereState :: Telomere -> TelomereState
telomereState t
  | repeats t <= 0                   = Crisis
  | repeats t <= criticalLength `div` 2 = Senescent
  | repeats t <= criticalLength      = Critical
  | repeats t <= initialRepeatCount `div` 2 = Shortened
  | otherwise                        = Healthy

-- | How many more divisions can this telomere support?
remainingDivisions :: Telomere -> Int
remainingDivisions t = case telomerase t of
  HighTelomerase -> maxBound  -- Effectively infinite
  LowTelomerase  -> (repeats t - criticalLength `div` 2)
                    `div` max 1 (shorteningPerDivision `div` 2)
  NoTelomerase   -> (repeats t - criticalLength `div` 2)
                    `div` max 1 shorteningPerDivision

-- | Get the repeat count
repeatCount :: Telomere -> Int
repeatCount = repeats

-- | Get telomere length in base pairs
basePairLength :: Telomere -> Int
basePairLength t = repeats t * length ttagggRepeat

-- | Is the telomere at critical length?
isCritical :: Telomere -> Bool
isCritical t = telomereState t `elem` [Critical, Senescent, Crisis]

-- | Has the telomere reached senescence?
isSenescent :: Telomere -> Bool
isSenescent t = telomereState t `elem` [Senescent, Crisis]

-- | Simulate complete cellular lifespan: divide until senescence
simulateLifespan :: Telomere -> [(Int, Telomere)]
simulateLifespan t = go 0 t
  where
    go n tel = (n, tel) : case divide tel of
      Nothing   -> []
      Just tel' -> if isSenescent tel' then [(n + 1, tel')] else go (n + 1) tel'

-- | Simulate lifespan with periodic telomerase activation
simulateWithTelomerase :: Int       -- ^ Divisions between telomerase pulses
                       -> Int       -- ^ Repeats added per pulse
                       -> Telomere -> [(Int, Telomere)]
simulateWithTelomerase interval amount t = go 0 t
  where
    go n tel =
      let tel' = if n > 0 && n `mod` interval == 0
                 then telomeraseExtend amount tel
                 else tel
      in (n, tel') : case divide tel' of
           Nothing    -> []
           Just tel'' -> if isSenescent tel''
                         then [(n + 1, tel'')]
                         else go (n + 1) tel''

-- | Display telomere status
showTelomere :: Telomere -> String
showTelomere t = unlines
  [ "Telomere Status:"
  , "  Repeats:    " ++ show (repeats t) ++ " (" ++ show (basePairLength t) ++ " bp)"
  , "  Divisions:  " ++ show (divisionsElapsed t)
  , "  State:      " ++ show (telomereState t)
  , "  Telomerase: " ++ show (telomerase t)
  , "  Shelterin:  " ++ if shelterinIntact t then "intact" else "disrupted"
  , "  Remaining:  " ++ if telomerase t == HighTelomerase
                        then "unlimited"
                        else show (remainingDivisions t) ++ " divisions"
  ]

-- | Display lifespan simulation results
showLifespan :: [(Int, Telomere)] -> String
showLifespan steps = unlines $
  [ "=== Telomere Lifespan Simulation ==="
  , "Division | Repeats | State"
  , "---------+---------+----------"
  ] ++
  [ padRight 9 (show n) ++ "| " ++ padRight 8 (show (repeats t))
    ++ "| " ++ show (telomereState t)
  | (n, t) <- steps
  , n `mod` 10 == 0 || n == length steps - 1 || isCritical t
  ] ++
  [ "---------+---------+----------"
  , "Total divisions: " ++ show (fst (last steps))
  ]
  where
    padRight n s = s ++ replicate (max 0 (n - length s)) ' '
