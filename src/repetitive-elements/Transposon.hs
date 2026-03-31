{-
  Transposon.hs -- Cut-and-paste and copy-and-paste transposition mechanics

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Repetitive Elements as Self-Modifying Code
-}

module Transposon
  ( Genome
  , GenomeLocus(..)
  , mkGenome
  , genomeSequence
  , genomeLength
  , showGenome
  -- Cut-and-paste
  , excise
  , insertAt
  , cutAndPaste
  -- Copy-and-paste
  , copyElement
  , copyAndPaste
  -- TIR recognition
  , isTIR
  , findTIRs
  -- Target site duplication
  , insertWithTSD
  -- Retrotransposition (TPRT)
  , reverseTranscribe
  , targetPrimedRT
  -- Domestication
  , domesticate
  -- Utility
  , genomeSlice
  ) where

import Repeat

-- | A genome is a sequence of nucleotides with optional annotations.
data Genome = Genome
  { _genomeSeq   :: Sequence
  , _genomeName  :: String
  } deriving (Eq)

instance Show Genome where
  show g = _genomeName g ++ " [" ++ show (genomeLength g) ++ " bp]"

-- | A locus within the genome (start position, length).
data GenomeLocus = GenomeLocus
  { locusStart  :: Int
  , locusLength :: Int
  } deriving (Show, Eq)

-- | Create a genome from a string.
mkGenome :: String -> String -> Genome
mkGenome name seqStr = Genome (mkSequence seqStr) name

-- | Get the raw sequence.
genomeSequence :: Genome -> Sequence
genomeSequence = _genomeSeq

-- | Genome length in base pairs.
genomeLength :: Genome -> Int
genomeLength = length . _genomeSeq

-- | Show genome as a nucleotide string.
showGenome :: Genome -> String
showGenome = showSequence . _genomeSeq

-- | Extract a slice of the genome.
genomeSlice :: Int -> Int -> Genome -> Sequence
genomeSlice start len g = take len (drop start (_genomeSeq g))

-- ─── Cut-and-Paste Transposition ────────────────────────────────────

-- | Excise a segment from the genome.
--   Returns (excised element, genome with gap).
excise :: Int -> Int -> Genome -> Maybe (Sequence, Genome)
excise start len g
  | start < 0 || start + len > genomeLength g = Nothing
  | otherwise =
    let seq_ = _genomeSeq g
        element = take len (drop start seq_)
        remaining = take start seq_ ++ drop (start + len) seq_
    in Just (element, g { _genomeSeq = remaining })

-- | Insert a sequence at a given position in the genome.
insertAt :: Int -> Sequence -> Genome -> Maybe Genome
insertAt pos element g
  | pos < 0 || pos > genomeLength g = Nothing
  | otherwise =
    let seq_ = _genomeSeq g
        newSeq = take pos seq_ ++ element ++ drop pos seq_
    in Just (g { _genomeSeq = newSeq })

-- | Cut-and-paste transposition: excise from one site, insert at another.
--   Preserves genome length (minus TSD contribution).
cutAndPaste :: Int -> Int -> Int -> Genome -> Maybe Genome
cutAndPaste from len to_ g = do
  (element, g') <- excise from len g
  -- Adjust target position if it was after the excision site
  let adjustedTo = if to_ > from then to_ - len else to_
  insertAt adjustedTo element g'

-- ─── Copy-and-Paste Retrotransposition ──────────────────────────────

-- | Copy an element from the genome (does not remove original).
copyElement :: Int -> Int -> Genome -> Maybe Sequence
copyElement start len g
  | start < 0 || start + len > genomeLength g = Nothing
  | otherwise = Just (take len (drop start (_genomeSeq g)))

-- | Copy-and-paste retrotransposition: copy from source, insert at target.
--   Genome grows by the length of the element.
copyAndPaste :: Int -> Int -> Int -> Genome -> Maybe Genome
copyAndPaste from len to_ g = do
  element <- copyElement from len g
  insertAt to_ element g

-- ─── TIR Recognition ───────────────────────────────────────────────

-- | Check if two sequences form a terminal inverted repeat pair.
--   TIR_right should be the reverse complement of TIR_left.
isTIR :: Sequence -> Sequence -> Bool
isTIR left right = right == reverseComplement left

-- | Find all positions where a given TIR pair occurs in the genome.
findTIRs :: Sequence -> Genome -> [(Int, Int)]
findTIRs tir g =
  let seq_ = _genomeSeq g
      tirLen = length tir
      revTir = reverseComplement tir
      leftPositions = findSubseq tir seq_ 0
      rightPositions = findSubseq revTir seq_ 0
  in [(l, r) | l <- leftPositions, r <- rightPositions, r > l + tirLen]
  where
    findSubseq _ [] _ = []
    findSubseq pat s@(_:rest) i
      | take (length pat) s == pat = i : findSubseq pat rest (i + 1)
      | otherwise = findSubseq pat rest (i + 1)

-- ─── Target Site Duplication ────────────────────────────────────────

-- | Insert an element with target site duplication.
--   The TSD is a short sequence at the insertion site that gets duplicated.
insertWithTSD :: Int -> Int -> Sequence -> Genome -> Maybe Genome
insertWithTSD pos tsdLen element g
  | pos < 0 || pos + tsdLen > genomeLength g = Nothing
  | otherwise =
    let seq_ = _genomeSeq g
        tsd = take tsdLen (drop pos seq_)
        newSeq = take pos seq_ ++ tsd ++ element ++ tsd ++ drop (pos + tsdLen) seq_
    in Just (g { _genomeSeq = newSeq })

-- ─── Target-Primed Reverse Transcription ────────────────────────────

-- | Simulate reverse transcription (identity in this model, since we
--   work at the DNA level; in reality RNA -> cDNA).
reverseTranscribe :: Sequence -> Sequence
reverseTranscribe = id  -- simplified: DNA -> RNA -> cDNA ~ identity

-- | Target-primed reverse transcription: the full L1 retrotransposition cycle.
--   1. Copy the element (transcription + reverse transcription)
--   2. Nick the target site
--   3. Insert with TSD
targetPrimedRT :: Int -> Int -> Int -> Genome -> Maybe Genome
targetPrimedRT from len target g = do
  element <- copyElement from len g
  let cDNA = reverseTranscribe element
  insertWithTSD target 15 cDNA g  -- L1 creates ~15 bp TSDs

-- ─── Domestication ─────────────────────────────────────────────────

-- | Domesticate a transposable element: convert it from a mobile element
--   to a host gene with a specific function.
domesticate :: Repeat -> String -> Repeat
domesticate (DNATransposon tl tr body tp _) func =
  DNATransposon tl tr body tp (Domesticated func)
domesticate (Retrotransposon cls body rt pa _) func =
  Retrotransposon cls body rt pa (Domesticated func)
domesticate (ERV ll lr g p e _ _) func =
  ERV ll lr g p e False (Domesticated func)
domesticate other _ = other  -- satellites cannot be domesticated in this sense
