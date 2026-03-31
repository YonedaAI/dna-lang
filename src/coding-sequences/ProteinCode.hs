{-
  ProteinCode.hs — Core type definitions for DNA-Lang coding sequences

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Coding Sequences as Executable Functions

  This module defines the fundamental types for representing the central
  dogma of molecular biology as a typed compilation pipeline:
    DNA (source code) -> mRNA (intermediate representation) -> Protein (executable)
-}

module ProteinCode
  ( Nucleotide(..)
  , AminoAcid(..)
  , Exon(..)
  , Intron(..)
  , GeneStructure(..)
  , ProteinCode(..)
  , MutationType(..)
  , SplicingContext(..)
  , PostTranslationalMod(..)
  , DNASequence
  , RNASequence
  , Codon
  , Protein
  , showNucleotide
  , showNucleotides
  , showAminoAcid
  , showProtein
  , showCodon
  , parseDNA
  , parseRNA
  ) where

-- | The four DNA nucleotides plus Uracil for RNA.
data Nucleotide = A | C | G | T | U
  deriving (Eq, Ord)

instance Show Nucleotide where
  show A = "A"
  show C = "C"
  show G = "G"
  show T = "T"
  show U = "U"

-- | The twenty standard amino acids plus a Stop signal.
data AminoAcid
  = Ala | Arg | Asn | Asp | Cys
  | Gln | Glu | Gly | His | Ile
  | Leu | Lys | Met | Phe | Pro
  | Ser | Thr | Trp | Tyr | Val
  | Stop
  deriving (Eq, Ord, Show)

-- | Type aliases for clarity.
type DNASequence = [Nucleotide]
type RNASequence = [Nucleotide]
type Codon = (Nucleotide, Nucleotide, Nucleotide)
type Protein = [AminoAcid]

-- | An exon is a coding region with start/end positions and its sequence.
data Exon = Exon
  { exonId    :: Int
  , exonStart :: Int
  , exonEnd   :: Int
  , exonSeq   :: [Nucleotide]
  } deriving (Show, Eq)

-- | An intron is a non-coding region between exons.
data Intron = Intron
  { intronId    :: Int
  , intronStart :: Int
  , intronEnd   :: Int
  , intronSeq   :: [Nucleotide]
  } deriving (Show, Eq)

-- | The structure of a gene with alternating exons and introns.
data GeneStructure = GeneStructure
  { gsName    :: String
  , gsExons   :: [Exon]
  , gsIntrons :: [Intron]
  } deriving (Show, Eq)

-- | The core ProteinCode type, parameterized by the product type.
--   Represents a complete coding sequence along with its compiled product.
data ProteinCode a = ProteinCode
  { pcGene    :: [Nucleotide]   -- ^ The source DNA sequence
  , pcProduct :: a              -- ^ The compiled product (e.g., Protein)
  , pcExons   :: [Exon]         -- ^ Exon structure of the gene
  , pcName    :: String         -- ^ Human-readable gene name
  } deriving (Show, Eq)

-- | Classification of mutations by their type-theoretic impact.
data MutationType
  = Silent          -- ^ Synonymous mutation: identity refactoring (same amino acid)
  | Missense        -- ^ Type-compatible substitution: different amino acid
  | Nonsense        -- ^ Type error: premature stop codon (termination)
  | Frameshift      -- ^ Parse error: insertion/deletion shifts reading frame
  deriving (Show, Eq)

-- | Cellular context for alternative splicing decisions.
data SplicingContext
  = Neural          -- ^ Brain/nervous tissue context
  | Muscle          -- ^ Muscle tissue context
  | Epithelial      -- ^ Epithelial tissue context
  | Hepatic         -- ^ Liver tissue context
  | DefaultContext   -- ^ Default/ubiquitous context
  deriving (Show, Eq)

-- | Post-translational modifications as runtime decorators.
data PostTranslationalMod
  = Phosphorylation Int     -- ^ Phosphorylation at given residue position
  | Glycosylation Int       -- ^ Glycosylation at given residue position
  | Ubiquitination Int      -- ^ Ubiquitination at given residue position
  | Acetylation Int         -- ^ Acetylation at given residue position
  | Methylation Int         -- ^ Methylation at given residue position
  deriving (Show, Eq)

-- | Display a single nucleotide as a character.
showNucleotide :: Nucleotide -> Char
showNucleotide A = 'A'
showNucleotide C = 'C'
showNucleotide G = 'G'
showNucleotide T = 'T'
showNucleotide U = 'U'

-- | Display a nucleotide sequence as a string.
showNucleotides :: [Nucleotide] -> String
showNucleotides = map showNucleotide

-- | Display an amino acid as its three-letter abbreviation.
showAminoAcid :: AminoAcid -> String
showAminoAcid = show

-- | Display a protein as a chain of amino acid abbreviations.
showProtein :: Protein -> String
showProtein = unwords . map showAminoAcid

-- | Display a codon as a three-character string.
showCodon :: Codon -> String
showCodon (a, b, c) = map showNucleotide [a, b, c]

-- | Parse a string of characters into a DNA sequence.
parseDNA :: String -> Maybe DNASequence
parseDNA = mapM parseNucDNA
  where
    parseNucDNA 'A' = Just A
    parseNucDNA 'C' = Just C
    parseNucDNA 'G' = Just G
    parseNucDNA 'T' = Just T
    parseNucDNA 'a' = Just A
    parseNucDNA 'c' = Just C
    parseNucDNA 'g' = Just G
    parseNucDNA 't' = Just T
    parseNucDNA _   = Nothing

-- | Parse a string of characters into an RNA sequence.
parseRNA :: String -> Maybe RNASequence
parseRNA = mapM parseNucRNA
  where
    parseNucRNA 'A' = Just A
    parseNucRNA 'C' = Just C
    parseNucRNA 'G' = Just G
    parseNucRNA 'U' = Just U
    parseNucRNA 'a' = Just A
    parseNucRNA 'c' = Just C
    parseNucRNA 'g' = Just G
    parseNucRNA 'u' = Just U
    parseNucRNA _   = Nothing
