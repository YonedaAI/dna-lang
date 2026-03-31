{-
  Transcription.hs — DNA to mRNA conversion

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Coding Sequences as Executable Functions

  Transcription is the first compilation pass in the central dogma:
    DNA (source code) -> mRNA (intermediate representation)

  The template (antisense) strand of DNA is read 3'->5', producing
  mRNA in the 5'->3' direction. For the coding (sense) strand,
  transcription is equivalent to replacing T with U.
-}

module Transcription
  ( transcribe
  , transcribeSense
  , reverseComplement
  , complement
  , dnaComplement
  , spliceMRNA
  , transcribeGene
  , selectExons
  ) where

import ProteinCode

-- | Transcribe a DNA sense strand to mRNA.
--   This replaces every T with U, preserving A, C, G.
--   Returns Nothing if the input contains invalid nucleotides (e.g., U in DNA).
transcribeSense :: DNASequence -> Maybe RNASequence
transcribeSense = mapM convertToRNA
  where
    convertToRNA :: Nucleotide -> Maybe Nucleotide
    convertToRNA A = Just A
    convertToRNA C = Just C
    convertToRNA G = Just G
    convertToRNA T = Just U
    convertToRNA U = Nothing  -- U should not appear in DNA

-- | Transcribe: given the template (antisense) strand, produce mRNA.
--   The template strand is read and complemented: A->U, T->A, C->G, G->C.
transcribe :: DNASequence -> Maybe RNASequence
transcribe = mapM templateToRNA
  where
    templateToRNA :: Nucleotide -> Maybe Nucleotide
    templateToRNA T = Just A
    templateToRNA A = Just U
    templateToRNA C = Just G
    templateToRNA G = Just C
    templateToRNA U = Nothing

-- | Compute the DNA complement: A<->T, C<->G.
dnaComplement :: Nucleotide -> Maybe Nucleotide
dnaComplement A = Just T
dnaComplement T = Just A
dnaComplement C = Just G
dnaComplement G = Just C
dnaComplement U = Nothing

-- | Complement an entire DNA sequence.
complement :: DNASequence -> Maybe DNASequence
complement = mapM dnaComplement

-- | Reverse complement of a DNA sequence.
reverseComplement :: DNASequence -> Maybe DNASequence
reverseComplement = fmap reverse . complement

-- | Splice an mRNA by removing intron regions and joining exons.
--   Takes a list of exons (with sequences) and concatenates them in order.
spliceMRNA :: [Exon] -> RNASequence
spliceMRNA exons = concatMap exonSeq (sortExons exons)
  where
    sortExons = foldr insertSorted []
    insertSorted e [] = [e]
    insertSorted e (x:xs)
      | exonId e <= exonId x = e : x : xs
      | otherwise            = x : insertSorted e xs

-- | Transcribe a complete gene, including splicing.
--   Takes a GeneStructure and a SplicingContext, returns the mature mRNA.
transcribeGene :: GeneStructure -> SplicingContext -> Maybe RNASequence
transcribeGene gs ctx =
  let selectedExons = selectExons (gsExons gs) ctx
      preMRNA = concatMap exonSeq (gsExons gs)
  in  case transcribeSenseSeq preMRNA of
        Nothing  -> Nothing
        Just rna -> Just (spliceMRNA selectedExons)
  where
    transcribeSenseSeq = mapM (\n -> case n of
      A -> Just A; C -> Just C; G -> Just G; T -> Just U; U -> Nothing)

-- | Select exons based on splicing context (alternative splicing).
selectExons :: [Exon] -> SplicingContext -> [Exon]
selectExons exons DefaultContext = exons
selectExons exons Neural =
  -- In neural context, include all exons (example: include brain-specific exon)
  exons
selectExons exons Muscle =
  -- In muscle context, skip exon 3 if present (example tissue-specific splicing)
  filter (\e -> exonId e /= 3) exons
selectExons exons Epithelial =
  -- In epithelial context, skip exons 2 and 4 if present
  filter (\e -> exonId e `notElem` [2, 4]) exons
selectExons exons Hepatic =
  -- In hepatic context, include only odd-numbered exons
  filter (\e -> odd (exonId e)) exons
