{-
  State.hs -- Core epigenetic state types for chromatin accessibility

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Epigenetic Marks as Runtime State

  This module defines the fundamental State type that tracks
  methylation status, histone modifications, and computed
  chromatin accessibility for genomic regions.
-}

module State
  ( EpigeneticState(..)
  , AccessibilityLevel(..)
  , ChromatinRegion(..)
  , GenomicPosition
  , RegionName
  , emptyState
  , getAccessibility
  , setAccessibility
  , isAccessible
  , isRepressed
  , isBivalent
  , combineStates
  , stateTransition
  , showState
  , showAccessibility
  , showRegion
  ) where

import qualified Methylation as M
import qualified HistoneModification as H

-- | Genomic position (base pair coordinate)
type GenomicPosition = Int

-- | Human-readable region name
type RegionName = String

-- | Accessibility levels for chromatin regions
data AccessibilityLevel
  = FullyOpen          -- ^ Euchromatin, actively transcribed
  | PartiallyOpen      -- ^ Poised or primed state
  | Bivalent           -- ^ Both activating and repressing marks
  | PartiallyRepressed -- ^ Partially silenced
  | FullyRepressed     -- ^ Heterochromatin, silenced
  | ConstitutiveHet    -- ^ Constitutive heterochromatin (pericentromeric)
  deriving (Eq, Ord, Show, Read)

-- | A chromatin region with position information
data ChromatinRegion = ChromatinRegion
  { regionName  :: RegionName
  , regionStart :: GenomicPosition
  , regionEnd   :: GenomicPosition
  } deriving (Eq, Show, Read)

-- | Core epigenetic state type parameterized by accessibility
data EpigeneticState a = EpigeneticState
  { methylation   :: M.MethylationMap
  , histones      :: H.HistoneMap
  , accessibility :: a
  } deriving (Eq, Show, Read)

-- | Functor instance: map over the accessibility component
instance Functor EpigeneticState where
  fmap f st = st { accessibility = f (accessibility st) }

-- | Applicative instance for EpigeneticState
instance Applicative EpigeneticState where
  pure a = EpigeneticState
    { methylation   = M.emptyMethylationMap
    , histones      = H.emptyHistoneMap
    , accessibility = a
    }
  sf <*> sa = EpigeneticState
    { methylation   = M.combineMethylationMaps (methylation sf) (methylation sa)
    , histones      = H.combineHistoneMaps (histones sf) (histones sa)
    , accessibility = (accessibility sf) (accessibility sa)
    }

-- | Monad instance for EpigeneticState
instance Monad EpigeneticState where
  return = pure
  sa >>= f =
    let result = f (accessibility sa)
    in EpigeneticState
      { methylation   = M.combineMethylationMaps (methylation sa) (methylation result)
      , histones      = H.combineHistoneMaps (histones sa) (histones result)
      , accessibility = accessibility result
      }

-- | Empty epigenetic state with default accessibility
emptyState :: EpigeneticState AccessibilityLevel
emptyState = EpigeneticState
  { methylation   = M.emptyMethylationMap
  , histones      = H.emptyHistoneMap
  , accessibility = PartiallyOpen
  }

-- | Get the accessibility level from a state
getAccessibility :: EpigeneticState a -> a
getAccessibility = accessibility

-- | Set the accessibility level
setAccessibility :: a -> EpigeneticState a -> EpigeneticState a
setAccessibility a st = st { accessibility = a }

-- | Check if a region is accessible (open or partially open)
isAccessible :: EpigeneticState AccessibilityLevel -> Bool
isAccessible st = case accessibility st of
  FullyOpen       -> True
  PartiallyOpen   -> True
  Bivalent        -> False
  _               -> False

-- | Check if a region is repressed
isRepressed :: EpigeneticState AccessibilityLevel -> Bool
isRepressed st = case accessibility st of
  FullyRepressed    -> True
  ConstitutiveHet   -> True
  PartiallyRepressed -> True
  _                  -> False

-- | Check if a region has bivalent chromatin
isBivalent :: EpigeneticState AccessibilityLevel -> Bool
isBivalent st = accessibility st == Bivalent

-- | Combine two epigenetic states, resolving accessibility
combineStates :: EpigeneticState AccessibilityLevel
              -> EpigeneticState AccessibilityLevel
              -> EpigeneticState AccessibilityLevel
combineStates s1 s2 = EpigeneticState
  { methylation   = M.combineMethylationMaps (methylation s1) (methylation s2)
  , histones      = H.combineHistoneMaps (histones s1) (histones s2)
  , accessibility = resolveAccessibility (accessibility s1) (accessibility s2)
  }

-- | Resolve two accessibility levels into one
resolveAccessibility :: AccessibilityLevel -> AccessibilityLevel -> AccessibilityLevel
resolveAccessibility FullyOpen FullyRepressed       = Bivalent
resolveAccessibility FullyRepressed FullyOpen        = Bivalent
resolveAccessibility ConstitutiveHet _               = ConstitutiveHet
resolveAccessibility _ ConstitutiveHet               = ConstitutiveHet
resolveAccessibility FullyRepressed _                = FullyRepressed
resolveAccessibility _ FullyRepressed                = FullyRepressed
resolveAccessibility FullyOpen _                     = FullyOpen
resolveAccessibility _ FullyOpen                     = FullyOpen
resolveAccessibility Bivalent _                      = Bivalent
resolveAccessibility _ Bivalent                      = Bivalent
resolveAccessibility PartiallyRepressed _            = PartiallyRepressed
resolveAccessibility _ PartiallyRepressed            = PartiallyRepressed
resolveAccessibility PartiallyOpen PartiallyOpen     = PartiallyOpen

-- | Apply a state transition function
stateTransition :: (EpigeneticState a -> EpigeneticState a)
                -> EpigeneticState a
                -> EpigeneticState a
stateTransition f = f

-- | Pretty-print an accessibility level
showAccessibility :: AccessibilityLevel -> String
showAccessibility FullyOpen          = "Fully Open (Euchromatin)"
showAccessibility PartiallyOpen      = "Partially Open (Poised)"
showAccessibility Bivalent           = "Bivalent (H3K4me3 + H3K27me3)"
showAccessibility PartiallyRepressed = "Partially Repressed"
showAccessibility FullyRepressed     = "Fully Repressed (Heterochromatin)"
showAccessibility ConstitutiveHet    = "Constitutive Heterochromatin"

-- | Pretty-print a chromatin region
showRegion :: ChromatinRegion -> String
showRegion r = regionName r ++ " [" ++ show (regionStart r)
            ++ "-" ++ show (regionEnd r) ++ "]"

-- | Pretty-print an epigenetic state
showState :: EpigeneticState AccessibilityLevel -> String
showState st = unlines
  [ "=== Epigenetic State ==="
  , "Methylation: " ++ M.showMethylationMap (methylation st)
  , "Histones:    " ++ H.showHistoneMap (histones st)
  , "Access:      " ++ showAccessibility (accessibility st)
  ]
