{-
  Repeat.hs -- Core types for repetitive genomic elements

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Repetitive Elements as Self-Modifying Code
-}

module Repeat
  ( Nucleotide(..)
  , Sequence
  , Gene(..)
  , ActivityState(..)
  , RTClass(..)
  , SatelliteLocation(..)
  , TranspositionMode(..)
  , HostEffect(..)
  , Repeat(..)
  , repeatLength
  , isActive
  , isMobile
  , isAutonomous
  , activityLabel
  , repeatSummary
  , showSequence
  , complement
  , reverseComplement
  , mkSequence
  ) where

-- | The four DNA nucleotides.
data Nucleotide = A | T | G | C
  deriving (Eq, Ord)

instance Show Nucleotide where
  show A = "A"
  show T = "T"
  show G = "G"
  show C = "C"

-- | A DNA sequence is a list of nucleotides.
type Sequence = [Nucleotide]

-- | Show a sequence as a compact string.
showSequence :: Sequence -> String
showSequence = concatMap show

-- | Parse a string into a sequence. Unknown characters are skipped.
mkSequence :: String -> Sequence
mkSequence [] = []
mkSequence (c:cs) = case c of
  'A' -> A : mkSequence cs
  'T' -> T : mkSequence cs
  'G' -> G : mkSequence cs
  'C' -> C : mkSequence cs
  'a' -> A : mkSequence cs
  't' -> T : mkSequence cs
  'g' -> G : mkSequence cs
  'c' -> C : mkSequence cs
  _   -> mkSequence cs

-- | Watson-Crick complement of a nucleotide.
complementNuc :: Nucleotide -> Nucleotide
complementNuc A = T
complementNuc T = A
complementNuc G = C
complementNuc C = G

-- | Complement of a sequence.
complement :: Sequence -> Sequence
complement = map complementNuc

-- | Reverse complement of a sequence.
reverseComplement :: Sequence -> Sequence
reverseComplement = reverse . complement

-- | A gene is a named coding sequence.
data Gene = Gene
  { geneName     :: String
  , geneSequence :: Sequence
  } deriving (Show, Eq)

-- | Activity states form a lattice: Active > Suppressed > Fossilized > Domesticated.
data ActivityState
  = Active
  | Suppressed
  | Fossilized
  | Domesticated String  -- carries the host function name
  deriving (Eq)

instance Show ActivityState where
  show Active            = "Active"
  show Suppressed        = "Suppressed"
  show Fossilized        = "Fossilized"
  show (Domesticated f)  = "Domesticated(" ++ f ++ ")"

-- | Classification of retrotransposons.
data RTClass = LINE | SINE
  deriving (Show, Eq, Ord)

-- | Location of satellite DNA.
data SatelliteLocation
  = Centromere
  | Telomere
  | Pericentromeric
  | Subtelomeric
  | Dispersed
  deriving (Show, Eq, Ord)

-- | Transposition mode.
data TranspositionMode = CutPaste | CopyPaste
  deriving (Show, Eq, Ord)

-- | Effect on the host organism.
data HostEffect
  = Neutral
  | Deleterious Double     -- fitness cost
  | Regulatory             -- provides regulatory function
  | Structural             -- structural role (centromere, telomere)
  | Exapted String         -- co-opted for named host function
  deriving (Show, Eq)

-- | The core Repeat type, parameterized by activity state.
data Repeat
  = DNATransposon
    { dtTirLeft     :: Sequence
    , dtTirRight    :: Sequence
    , dtBody        :: Sequence
    , dtTransposase :: Gene
    , dtActivity    :: ActivityState
    }
  | Retrotransposon
    { rtClass_              :: RTClass
    , rtBody                :: Sequence
    , rtReverseTranscriptase :: Maybe Gene
    , rtPolyA               :: Bool
    , rtActivity            :: ActivityState
    }
  | SatelliteDNA
    { satRepeatUnit  :: Sequence
    , satCopyNumber  :: Int
    , satLocation    :: SatelliteLocation
    }
  | ERV
    { ervLtrLeft   :: Sequence
    , ervLtrRight  :: Sequence
    , ervGag       :: Maybe Gene
    , ervPol       :: Maybe Gene
    , ervEnv       :: Maybe Gene
    , ervIntact    :: Bool
    , ervActivity  :: ActivityState
    }
  deriving (Show, Eq)

-- | Total length of a repeat element in base pairs.
repeatLength :: Repeat -> Int
repeatLength (DNATransposon tl tr body _ _) =
  length tl + length body + length tr
repeatLength (Retrotransposon _ body _ hasPolyA _) =
  length body + if hasPolyA then 100 else 0  -- approximate polyA tail
repeatLength (SatelliteDNA unit n _) =
  length unit * n
repeatLength (ERV ll lr g p e _ _) =
  length ll + length lr + geneLen g + geneLen p + geneLen e
  where geneLen Nothing  = 0
        geneLen (Just gene) = length (geneSequence gene)

-- | Check if a repeat element is currently active.
isActive :: Repeat -> Bool
isActive (DNATransposon _ _ _ _ Active) = True
isActive (Retrotransposon _ _ _ _ Active) = True
isActive (ERV _ _ _ _ _ _ Active) = True
isActive _ = False

-- | Check if a repeat element is capable of transposition (active and has machinery).
isMobile :: Repeat -> Bool
isMobile r@(DNATransposon{}) = isActive r
isMobile r@(Retrotransposon _ _ (Just _) _ _) = isActive r
isMobile _ = False

-- | Check if a retrotransposon is autonomous (encodes its own RT).
isAutonomous :: Repeat -> Bool
isAutonomous (Retrotransposon LINE _ (Just _) _ _) = True
isAutonomous _ = False

-- | Get the activity label.
activityLabel :: Repeat -> String
activityLabel (DNATransposon _ _ _ _ a) = show a
activityLabel (Retrotransposon _ _ _ _ a) = show a
activityLabel (SatelliteDNA{}) = "Structural"
activityLabel (ERV _ _ _ _ _ _ a) = show a

-- | One-line summary of a repeat element.
repeatSummary :: Repeat -> String
repeatSummary (DNATransposon _ _ body _ act) =
  "DNA Transposon [" ++ show (length body) ++ " bp, " ++ show act ++ "]"
repeatSummary (Retrotransposon cls body _ _ act) =
  show cls ++ " [" ++ show (length body) ++ " bp, " ++ show act ++ "]"
repeatSummary (SatelliteDNA unit n loc) =
  "Satellite (" ++ show loc ++ ") [" ++ show (length unit) ++ " bp x " ++ show n ++ " = " ++ show (length unit * n) ++ " bp]"
repeatSummary (ERV _ _ _ _ _ intact act) =
  "ERV [" ++ (if intact then "intact" else "degraded") ++ ", " ++ show act ++ "]"
