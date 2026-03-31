{-
  Translation.hs — mRNA to amino acid sequence (protein) translation

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Coding Sequences as Executable Functions

  Translation is the second compilation pass in the central dogma:
    mRNA (intermediate representation) -> Protein (executable binary)

  The ribosome reads mRNA codons (triplets) starting from AUG (start codon)
  and produces a chain of amino acids until encountering a stop codon.
-}

module Translation
  ( translate
  , translateFromStart
  , findStartCodon
  , extractCodons
  , classifyMutation
  , translateReadingFrame
  , allReadingFrames
  ) where

import ProteinCode
import CodonTable

-- | Extract codons (triplets) from an RNA sequence.
--   Discards any trailing nucleotides that don't form a complete codon.
extractCodons :: RNASequence -> [Codon]
extractCodons (a:b:c:rest) = (a, b, c) : extractCodons rest
extractCodons _            = []

-- | Find the position of the first AUG start codon in an RNA sequence.
--   Returns the index (0-based) of the A in AUG, or Nothing if not found.
findStartCodon :: RNASequence -> Maybe Int
findStartCodon rna = go rna 0
  where
    go (A:U:G:_) idx = Just idx
    go (_:rest)  idx = go rest (idx + 1)
    go []        _   = Nothing

-- | Translate an mRNA sequence starting from the first AUG codon.
--   Reads codons in the reading frame set by AUG and stops at the first
--   stop codon. Returns Nothing if no start codon is found.
translateFromStart :: RNASequence -> Maybe Protein
translateFromStart rna = case findStartCodon rna of
  Nothing  -> Nothing
  Just idx -> Just (translateCodons (extractCodons (drop idx rna)))

-- | Translate an mRNA sequence directly (assumes already in frame).
--   Reads codons and accumulates amino acids until a stop codon or end.
translate :: RNASequence -> Protein
translate = translateCodons . extractCodons

-- | Internal: translate a list of codons to amino acids.
--   Stops at the first Stop signal.
translateCodons :: [Codon] -> Protein
translateCodons [] = []
translateCodons (c:cs) = case codonToAminoAcid c of
  Nothing   -> []       -- Invalid codon: stop
  Just Stop -> []       -- Stop codon: terminate translation
  Just aa   -> aa : translateCodons cs

-- | Translate in a specific reading frame (0, 1, or 2).
translateReadingFrame :: Int -> RNASequence -> Protein
translateReadingFrame frame rna
  | frame < 0 || frame > 2 = []
  | otherwise               = translate (drop frame rna)

-- | Get all three forward reading frames of an RNA sequence.
allReadingFrames :: RNASequence -> [Protein]
allReadingFrames rna = map (\f -> translateReadingFrame f rna) [0, 1, 2]

-- | Classify a point mutation by comparing original and mutated codons.
--   Takes the original codon and the mutated codon, returns the mutation type.
classifyMutation :: Codon -> Codon -> MutationType
classifyMutation original mutated
  | original == mutated = Silent  -- No actual change
  | otherwise =
      let origAA = codonToAminoAcid original
          mutAA  = codonToAminoAcid mutated
      in case (origAA, mutAA) of
        (Just Stop, Just Stop) -> Silent
        (Just aa1, Just Stop)  -> Nonsense   -- Premature termination
        (Just aa1, Just aa2)
          | aa1 == aa2         -> Silent     -- Synonymous: same amino acid
          | otherwise          -> Missense   -- Different amino acid
        _                      -> Missense   -- Fallback
