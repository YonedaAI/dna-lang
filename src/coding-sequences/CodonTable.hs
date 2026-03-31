{-
  CodonTable.hs — Complete codon-to-amino-acid mapping (all 64 codons)

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Coding Sequences as Executable Functions
-}

module CodonTable
  ( codonToAminoAcid
  , isStartCodon
  , isStopCodon
  , allCodons
  , aminoAcidName
  , aminoAcidAbbrev3
  , aminoAcidAbbrev1
  , codonTableEntries
  ) where

import ProteinCode

-- | Look up the amino acid encoded by a three-nucleotide codon.
--   Returns Nothing if the input is not exactly three RNA nucleotides.
codonToAminoAcid :: (Nucleotide, Nucleotide, Nucleotide) -> Maybe AminoAcid
codonToAminoAcid codon = case codon of
  -- UUx codons
  (U, U, U) -> Just Phe
  (U, U, C) -> Just Phe
  (U, U, A) -> Just Leu
  (U, U, G) -> Just Leu
  -- UCx codons
  (U, C, U) -> Just Ser
  (U, C, C) -> Just Ser
  (U, C, A) -> Just Ser
  (U, C, G) -> Just Ser
  -- UAx codons
  (U, A, U) -> Just Tyr
  (U, A, C) -> Just Tyr
  (U, A, A) -> Just Stop  -- Ochre stop codon
  (U, A, G) -> Just Stop  -- Amber stop codon
  -- UGx codons
  (U, G, U) -> Just Cys
  (U, G, C) -> Just Cys
  (U, G, A) -> Just Stop  -- Opal stop codon
  (U, G, G) -> Just Trp
  -- CUx codons
  (C, U, U) -> Just Leu
  (C, U, C) -> Just Leu
  (C, U, A) -> Just Leu
  (C, U, G) -> Just Leu
  -- CCx codons
  (C, C, U) -> Just Pro
  (C, C, C) -> Just Pro
  (C, C, A) -> Just Pro
  (C, C, G) -> Just Pro
  -- CAx codons
  (C, A, U) -> Just His
  (C, A, C) -> Just His
  (C, A, A) -> Just Gln
  (C, A, G) -> Just Gln
  -- CGx codons
  (C, G, U) -> Just Arg
  (C, G, C) -> Just Arg
  (C, G, A) -> Just Arg
  (C, G, G) -> Just Arg
  -- AUx codons
  (A, U, U) -> Just Ile
  (A, U, C) -> Just Ile
  (A, U, A) -> Just Ile
  (A, U, G) -> Just Met  -- Also the START codon
  -- ACx codons
  (A, C, U) -> Just Thr
  (A, C, C) -> Just Thr
  (A, C, A) -> Just Thr
  (A, C, G) -> Just Thr
  -- AAx codons
  (A, A, U) -> Just Asn
  (A, A, C) -> Just Asn
  (A, A, A) -> Just Lys
  (A, A, G) -> Just Lys
  -- AGx codons
  (A, G, U) -> Just Ser
  (A, G, C) -> Just Ser
  (A, G, A) -> Just Arg
  (A, G, G) -> Just Arg
  -- GUx codons
  (G, U, U) -> Just Val
  (G, U, C) -> Just Val
  (G, U, A) -> Just Val
  (G, U, G) -> Just Val
  -- GCx codons
  (G, C, U) -> Just Ala
  (G, C, C) -> Just Ala
  (G, C, A) -> Just Ala
  (G, C, G) -> Just Ala
  -- GAx codons
  (G, A, U) -> Just Asp
  (G, A, C) -> Just Asp
  (G, A, A) -> Just Glu
  (G, A, G) -> Just Glu
  -- GGx codons
  (G, G, U) -> Just Gly
  (G, G, C) -> Just Gly
  (G, G, A) -> Just Gly
  (G, G, G) -> Just Gly
  -- DNA nucleotides in codon (invalid for translation)
  _         -> Nothing

-- | Check whether a codon is the standard start codon (AUG).
isStartCodon :: (Nucleotide, Nucleotide, Nucleotide) -> Bool
isStartCodon (A, U, G) = True
isStartCodon _         = False

-- | Check whether a codon is a stop codon (UAA, UAG, UGA).
isStopCodon :: (Nucleotide, Nucleotide, Nucleotide) -> Bool
isStopCodon (U, A, A) = True
isStopCodon (U, A, G) = True
isStopCodon (U, G, A) = True
isStopCodon _         = False

-- | All 64 RNA codons in standard order.
allCodons :: [(Nucleotide, Nucleotide, Nucleotide)]
allCodons = [(a, b, c) | a <- rnaBases, b <- rnaBases, c <- rnaBases]
  where rnaBases = [U, C, A, G]

-- | Full name of an amino acid.
aminoAcidName :: AminoAcid -> String
aminoAcidName aa = case aa of
  Ala  -> "Alanine"
  Arg  -> "Arginine"
  Asn  -> "Asparagine"
  Asp  -> "Aspartic acid"
  Cys  -> "Cysteine"
  Gln  -> "Glutamine"
  Glu  -> "Glutamic acid"
  Gly  -> "Glycine"
  His  -> "Histidine"
  Ile  -> "Isoleucine"
  Leu  -> "Leucine"
  Lys  -> "Lysine"
  Met  -> "Methionine"
  Phe  -> "Phenylalanine"
  Pro  -> "Proline"
  Ser  -> "Serine"
  Thr  -> "Threonine"
  Trp  -> "Tryptophan"
  Tyr  -> "Tyrosine"
  Val  -> "Valine"
  Stop -> "Stop"

-- | Three-letter amino acid abbreviation.
aminoAcidAbbrev3 :: AminoAcid -> String
aminoAcidAbbrev3 aa = case aa of
  Ala  -> "Ala"; Arg  -> "Arg"; Asn  -> "Asn"; Asp  -> "Asp"
  Cys  -> "Cys"; Gln  -> "Gln"; Glu  -> "Glu"; Gly  -> "Gly"
  His  -> "His"; Ile  -> "Ile"; Leu  -> "Leu"; Lys  -> "Lys"
  Met  -> "Met"; Phe  -> "Phe"; Pro  -> "Pro"; Ser  -> "Ser"
  Thr  -> "Thr"; Trp  -> "Trp"; Tyr  -> "Tyr"; Val  -> "Val"
  Stop -> "***"

-- | Single-letter amino acid code.
aminoAcidAbbrev1 :: AminoAcid -> Char
aminoAcidAbbrev1 aa = case aa of
  Ala  -> 'A'; Arg  -> 'R'; Asn  -> 'N'; Asp  -> 'D'
  Cys  -> 'C'; Gln  -> 'Q'; Glu  -> 'E'; Gly  -> 'G'
  His  -> 'H'; Ile  -> 'I'; Leu  -> 'L'; Lys  -> 'K'
  Met  -> 'M'; Phe  -> 'F'; Pro  -> 'P'; Ser  -> 'S'
  Thr  -> 'T'; Trp  -> 'W'; Tyr  -> 'Y'; Val  -> 'V'
  Stop -> '*'

-- | All 64 codon table entries as (codon-string, amino-acid) pairs.
codonTableEntries :: [((Nucleotide, Nucleotide, Nucleotide), AminoAcid)]
codonTableEntries =
  [ (c, aa) | c <- allCodons, Just aa <- [codonToAminoAcid c] ]
