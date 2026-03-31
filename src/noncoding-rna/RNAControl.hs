{-
  RNAControl.hs — Core parametric type for non-coding RNA control

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Non-Coding RNAs as Signals and Middleware
-}

module RNAControl
  ( RNAControl(..)
  , LncRNARole(..)
  , SubunitType(..)
  , RISCResult(..)
  , Nucleotide(..)
  , complement
  , isComplement
  , isWobble
  , reverseComplement
  , hammingDistance
  , toRNA
  , showNucleotide
  , parseNucleotide
  , parseSequence
  ) where

-- | RNA nucleotides
data Nucleotide = A | U | G | C
  deriving (Eq, Ord)

instance Show Nucleotide where
  show A = "A"
  show U = "U"
  show G = "G"
  show C = "C"

-- | Parse a character to a nucleotide
parseNucleotide :: Char -> Maybe Nucleotide
parseNucleotide 'A' = Just A
parseNucleotide 'U' = Just U
parseNucleotide 'G' = Just G
parseNucleotide 'C' = Just C
parseNucleotide 'a' = Just A
parseNucleotide 'u' = Just U
parseNucleotide 'g' = Just G
parseNucleotide 'c' = Just C
parseNucleotide _   = Nothing

-- | Show a nucleotide as a character
showNucleotide :: Nucleotide -> Char
showNucleotide A = 'A'
showNucleotide U = 'U'
showNucleotide G = 'G'
showNucleotide C = 'C'

-- | Parse a string into a nucleotide sequence
parseSequence :: String -> Maybe [Nucleotide]
parseSequence = mapM parseNucleotide

-- | Convert DNA thymine to RNA uracil
toRNA :: Char -> Char
toRNA 'T' = 'U'
toRNA 't' = 'u'
toRNA c   = c

-- | Watson-Crick complement
complement :: Nucleotide -> Nucleotide
complement A = U
complement U = A
complement G = C
complement C = G

-- | Check Watson-Crick complementarity
isComplement :: Nucleotide -> Nucleotide -> Bool
isComplement a b = complement a == b

-- | Check wobble base pairing (G-U)
isWobble :: Nucleotide -> Nucleotide -> Bool
isWobble G U = True
isWobble U G = True
isWobble _ _ = False

-- | Reverse complement of a nucleotide sequence
reverseComplement :: [Nucleotide] -> [Nucleotide]
reverseComplement = reverse . map complement

-- | Hamming distance between two sequences of equal length
hammingDistance :: (Eq a) => [a] -> [a] -> Int
hammingDistance xs ys = length (filter id (zipWith (/=) xs ys))

-- | Roles that lncRNAs can play
data LncRNARole
  = Scaffold    -- ^ Assembles protein complexes
  | Guide       -- ^ Recruits complexes to specific loci
  | Decoy       -- ^ Sequesters transcription factors or miRNAs
  | Enhancer    -- ^ Enhancer-derived, facilitates gene activation
  | Sponge      -- ^ Sequesters miRNAs (e.g., circular RNAs)
  deriving (Show, Eq)

-- | Ribosomal subunit types
data SubunitType
  = SmallSubunit   -- ^ 40S (18S rRNA) in eukaryotes
  | LargeSubunit   -- ^ 60S (28S + 5.8S + 5S rRNA) in eukaryotes
  deriving (Show, Eq)

-- | Result of RISC-mediated targeting
data RISCResult
  = Cleave    -- ^ Endonucleolytic cleavage (siRNA mode, perfect match)
  | Repress   -- ^ Translational repression (miRNA mode, partial match)
  | Pass      -- ^ No match, target unaffected
  deriving (Show, Eq)

-- | The core RNAControl type, parameterized by the controlled process type.
--
--   Each constructor corresponds to a major class of non-coding RNA:
--     MiRNA  — guard clause (post-transcriptional repression)
--     SiRNA  — exception handler (RNA interference / degradation)
--     LncRNA — middleware / scaffold (complex assembly)
--     TRNA   — natural transformation (codon-to-amino-acid adaptor)
--     RRNA   — virtual machine (ribosome execution engine)
--     PiRNA  — security monitor (transposon silencing)
data RNAControl p
  = MiRNA
      { miSeed      :: [Nucleotide]  -- ^ Seed sequence (positions 2-8)
      , miTargets   :: [p]           -- ^ Target processes
      , miThreshold :: Double        -- ^ Repression threshold [0,1]
      }
  | SiRNA
      { siGuide       :: [Nucleotide]  -- ^ Guide strand (~21nt)
      , siPassenger   :: [Nucleotide]  -- ^ Passenger strand
      , siMismatchTol :: Int           -- ^ Mismatch tolerance (0 for siRNA)
      }
  | LncRNA
      { lncSequence :: [Nucleotide]  -- ^ Full sequence
      , lncPartners :: [String]      -- ^ Binding partner names
      , lncRole     :: LncRNARole    -- ^ Functional role
      }
  | TRNA
      { trnaAnticodon :: [Nucleotide]  -- ^ Anticodon triplet
      , trnaAminoAcid :: Char          -- ^ Carried amino acid (1-letter code)
      }
  | RRNA
      { rrnaSubunit :: SubunitType  -- ^ Ribosomal subunit membership
      }
  | PiRNA
      { piGuide  :: [Nucleotide]  -- ^ piRNA guide sequence (24-31nt)
      , piTarget :: String        -- ^ Target transposon family
      }
  deriving (Show, Eq)
