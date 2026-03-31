{-
  Centromere.hs — Synchronization primitives for chromosome segregation

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Structural DNA as Memory Architecture

  Centromeres are formalized as synchronization primitives: the
  kinetochore serves as a mutex for microtubule attachment, and
  the spindle assembly checkpoint (SAC) acts as a barrier that
  ensures all chromosomes are properly attached before anaphase.
-}

module Centromere
  ( -- * Core types
    Centromere(..)
  , Kinetochore(..)
  , SpindleCheckpoint(..)
  , AttachmentState(..)
  , CheckpointSignal(..)
    -- * Construction
  , newCentromere
  , newKinetochore
    -- * Kinetochore operations (mutex semantics)
  , attachMicrotubule
  , detachMicrotubule
  , isProperlyAttached
  , isMerotelic
    -- * Spindle checkpoint (barrier semantics)
  , newCheckpoint
  , checkpointSatisfied
  , signalCheckpoint
  , evaluateCheckpoint
    -- * Chromosome set operations
  , allAttached
  , readyForAnaphase
  , segregate
    -- * Display
  , showCentromere
  , showCheckpoint
  ) where

-- | Microtubule attachment state for a kinetochore
data AttachmentState
  = Unattached           -- ^ No microtubule connection
  | Monotelic            -- ^ One kinetochore attached (one side)
  | Syntelic             -- ^ Both kinetochores to same pole (error)
  | Amphitelic           -- ^ Correct: each kinetochore to opposite pole
  | Merotelic            -- ^ Error: one kinetochore to both poles
  deriving (Show, Eq, Ord)

-- | Checkpoint signaling state
data CheckpointSignal
  = Wait                 -- ^ Mad2 active, block anaphase
  | Proceed              -- ^ All clear, allow anaphase
  | Error String         -- ^ Attachment error detected
  deriving (Show, Eq)

-- | A kinetochore: the protein complex at the centromere that
--   mediates microtubule attachment. Models a mutex.
data Kinetochore = Kinetochore
  { kAttachment    :: AttachmentState
  , kMicrotubules  :: Int          -- ^ Number of attached microtubules
  , kTension       :: Double       -- ^ Tension from bipolar attachment
  , kMad2Active    :: Bool         -- ^ Mad2 checkpoint protein active
  } deriving (Show, Eq)

-- | A centromere with its pair of kinetochores
data Centromere = Centromere
  { centroName       :: String
  , centroChromosome :: Int
  , kinetochoreA     :: Kinetochore   -- ^ Kinetochore facing pole A
  , kinetochoreB     :: Kinetochore   -- ^ Kinetochore facing pole B
  , alphaRepeats     :: Int           -- ^ Alpha-satellite repeat count
  , cenpaLoaded      :: Bool          -- ^ CENP-A histone variant present
  } deriving (Show, Eq)

-- | Spindle assembly checkpoint: a barrier synchronization primitive.
--   All chromosomes must be properly attached before the barrier releases.
data SpindleCheckpoint = SpindleCheckpoint
  { checkpointCentromeres :: [Centromere]
  , checkpointSignal      :: CheckpointSignal
  , madProteinsActive     :: Int        -- ^ Count of active Mad2 signals
  , apcActive             :: Bool       -- ^ APC/C ubiquitin ligase active
  } deriving (Show, Eq)

-- | Create a new unattached kinetochore
newKinetochore :: Kinetochore
newKinetochore = Kinetochore
  { kAttachment   = Unattached
  , kMicrotubules = 0
  , kTension      = 0.0
  , kMad2Active   = True    -- Mad2 active until proper attachment
  }

-- | Create a new centromere for a given chromosome
newCentromere :: String -> Int -> Centromere
newCentromere name chrNum = Centromere
  { centroName       = name
  , centroChromosome = chrNum
  , kinetochoreA     = newKinetochore
  , kinetochoreB     = newKinetochore
  , alphaRepeats     = 1000
  , cenpaLoaded      = True
  }

-- | Attach a microtubule to a kinetochore (mutex acquire).
--   Returns the updated kinetochore with new attachment state.
attachMicrotubule :: Char -> Centromere -> Centromere
attachMicrotubule pole c =
  case pole of
    'A' -> let k = kinetochoreA c
               k' = k { kMicrotubules = kMicrotubules k + 1
                       , kAttachment   = Monotelic
                       , kMad2Active   = False
                       }
           in updateAttachmentState $ c { kinetochoreA = k' }
    'B' -> let k = kinetochoreB c
               k' = k { kMicrotubules = kMicrotubules k + 1
                       , kAttachment   = Monotelic
                       , kMad2Active   = False
                       }
           in updateAttachmentState $ c { kinetochoreB = k' }
    _   -> c

-- | Detach a microtubule (mutex release)
detachMicrotubule :: Char -> Centromere -> Centromere
detachMicrotubule pole c =
  case pole of
    'A' -> let k = kinetochoreA c
               n = max 0 (kMicrotubules k - 1)
               k' = k { kMicrotubules = n
                       , kAttachment   = if n == 0 then Unattached else kAttachment k
                       , kMad2Active   = n == 0
                       }
           in updateAttachmentState $ c { kinetochoreA = k' }
    'B' -> let k = kinetochoreB c
               n = max 0 (kMicrotubules k - 1)
               k' = k { kMicrotubules = n
                       , kAttachment   = if n == 0 then Unattached else kAttachment k
                       , kMad2Active   = n == 0
                       }
           in updateAttachmentState $ c { kinetochoreB = k' }
    _   -> c

-- | Update overall attachment state based on both kinetochores
updateAttachmentState :: Centromere -> Centromere
updateAttachmentState c =
  let aAtt = kMicrotubules (kinetochoreA c) > 0
      bAtt = kMicrotubules (kinetochoreB c) > 0
      tension = if aAtt && bAtt then 1.0 else 0.0
      stateA = if aAtt && bAtt then Amphitelic
               else if aAtt then Monotelic
               else Unattached
      stateB = stateA
      kA = (kinetochoreA c) { kAttachment = stateA, kTension = tension
                             , kMad2Active = not (aAtt && bAtt) }
      kB = (kinetochoreB c) { kAttachment = stateB, kTension = tension
                             , kMad2Active = not (aAtt && bAtt) }
  in c { kinetochoreA = kA, kinetochoreB = kB }

-- | Check if the centromere has proper amphitelic attachment
isProperlyAttached :: Centromere -> Bool
isProperlyAttached c =
  kAttachment (kinetochoreA c) == Amphitelic &&
  kAttachment (kinetochoreB c) == Amphitelic &&
  kTension (kinetochoreA c) > 0

-- | Check for merotelic attachment error
isMerotelic :: Centromere -> Bool
isMerotelic c =
  kAttachment (kinetochoreA c) == Merotelic ||
  kAttachment (kinetochoreB c) == Merotelic

-- | Create a new spindle checkpoint for a set of centromeres
newCheckpoint :: [Centromere] -> SpindleCheckpoint
newCheckpoint cs = SpindleCheckpoint
  { checkpointCentromeres = cs
  , checkpointSignal      = Wait
  , madProteinsActive     = length cs * 2  -- Two kinetochores each
  , apcActive             = False
  }

-- | Check if all centromeres satisfy the checkpoint
checkpointSatisfied :: SpindleCheckpoint -> Bool
checkpointSatisfied sc = all isProperlyAttached (checkpointCentromeres sc)

-- | Send a signal to the checkpoint after attachment
signalCheckpoint :: Centromere -> SpindleCheckpoint -> SpindleCheckpoint
signalCheckpoint c sc =
  let cs' = map (\old -> if centroName old == centroName c then c else old)
                (checkpointCentromeres sc)
      madCount = length [() | cent <- cs'
                            , kMad2Active (kinetochoreA cent)
                              || kMad2Active (kinetochoreB cent)]
  in sc { checkpointCentromeres = cs'
        , madProteinsActive     = madCount
        }

-- | Evaluate checkpoint: returns Proceed only if all are amphitelic
evaluateCheckpoint :: SpindleCheckpoint -> SpindleCheckpoint
evaluateCheckpoint sc
  | checkpointSatisfied sc =
      sc { checkpointSignal = Proceed, apcActive = True, madProteinsActive = 0 }
  | any isMerotelic (checkpointCentromeres sc) =
      sc { checkpointSignal = Error "Merotelic attachment detected" }
  | otherwise =
      sc { checkpointSignal = Wait }

-- | Check if all chromosomes in a set are properly attached
allAttached :: [Centromere] -> Bool
allAttached = all isProperlyAttached

-- | Full readiness check for anaphase
readyForAnaphase :: SpindleCheckpoint -> Bool
readyForAnaphase sc =
  checkpointSignal sc == Proceed && apcActive sc

-- | Simulate chromosome segregation (only if checkpoint passed)
segregate :: SpindleCheckpoint -> Maybe ([Centromere], [Centromere])
segregate sc
  | not (readyForAnaphase sc) = Nothing
  | otherwise =
      let cs = checkpointCentromeres sc
          poleA = cs  -- Each chromosome sends one chromatid to each pole
          poleB = cs  -- (simplified: in reality, sister chromatids separate)
      in Just (poleA, poleB)

-- | Display centromere status
showCentromere :: Centromere -> String
showCentromere c = unlines
  [ "Centromere: " ++ centroName c ++ " (chr" ++ show (centroChromosome c) ++ ")"
  , "  Kinetochore A: " ++ show (kAttachment (kinetochoreA c))
    ++ " (" ++ show (kMicrotubules (kinetochoreA c)) ++ " MTs)"
    ++ if kMad2Active (kinetochoreA c) then " [Mad2 ACTIVE]" else " [Mad2 silent]"
  , "  Kinetochore B: " ++ show (kAttachment (kinetochoreB c))
    ++ " (" ++ show (kMicrotubules (kinetochoreB c)) ++ " MTs)"
    ++ if kMad2Active (kinetochoreB c) then " [Mad2 ACTIVE]" else " [Mad2 silent]"
  , "  CENP-A: " ++ if cenpaLoaded c then "loaded" else "absent"
  , "  Properly attached: " ++ show (isProperlyAttached c)
  ]

-- | Display checkpoint status
showCheckpoint :: SpindleCheckpoint -> String
showCheckpoint sc = unlines
  [ "=== Spindle Assembly Checkpoint ==="
  , "Signal: " ++ show (checkpointSignal sc)
  , "Mad2 active signals: " ++ show (madProteinsActive sc)
  , "APC/C active: " ++ show (apcActive sc)
  , "Chromosomes: " ++ show (length (checkpointCentromeres sc))
  , "All attached: " ++ show (checkpointSatisfied sc)
  , "Ready for anaphase: " ++ show (readyForAnaphase sc)
  ]
