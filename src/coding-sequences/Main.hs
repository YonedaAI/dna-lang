{-
  Main.hs — Entry point for DNA-Lang coding sequences demonstration

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Coding Sequences as Executable Functions

  Demonstrates the typed compilation pipeline of the central dogma:
  1. Transcription (DNA -> mRNA)
  2. Translation (mRNA -> Protein)
  3. Codon table lookups
  4. Mutation classification
  5. Alternative splicing
-}

module Main where

import ProteinCode
import CodonTable
import Transcription (transcribeSense, selectExons)
import Translation

-- ============================================================
-- Utility: display helpers
-- ============================================================

divider :: String -> IO ()
divider title = do
  putStrLn ""
  putStrLn (replicate 60 '=')
  putStrLn ("  " ++ title)
  putStrLn (replicate 60 '=')
  putStrLn ""

-- ============================================================
-- Demo 1: Transcription
-- ============================================================

demoTranscription :: IO ()
demoTranscription = do
  divider "DEMO 1: Transcription (DNA -> mRNA)"

  -- A sample DNA coding (sense) strand encoding Met-Ala-Ser-Stop
  -- ATG GCT TCT TAA
  let dnaStr = "ATGGCTTCTTAA"
  putStrLn $ "DNA sense strand:  " ++ dnaStr

  case parseDNA dnaStr of
    Nothing -> putStrLn "ERROR: Could not parse DNA sequence."
    Just dna -> do
      putStrLn $ "Parsed DNA:        " ++ showNucleotides dna

      case transcribeSense dna of
        Nothing  -> putStrLn "ERROR: Transcription failed."
        Just rna -> do
          putStrLn $ "mRNA (transcribed): " ++ showNucleotides rna
          putStrLn ""
          putStrLn "Transcription rule: T -> U (sense strand to mRNA)"
          putStrLn "  A -> A,  C -> C,  G -> G,  T -> U"

  putStrLn ""

  -- A longer example: insulin signal peptide (first 24 nt, simplified)
  let insulinDNA = "ATGGCCCTGTGGATGCGCCTCCTG"
  putStrLn $ "Insulin signal (DNA):  " ++ insulinDNA
  case parseDNA insulinDNA >>= transcribeSense of
    Nothing  -> putStrLn "ERROR: Could not transcribe."
    Just rna -> putStrLn $ "Insulin signal (mRNA): " ++ showNucleotides rna

-- ============================================================
-- Demo 2: Translation
-- ============================================================

demoTranslation :: IO ()
demoTranslation = do
  divider "DEMO 2: Translation (mRNA -> Protein)"

  -- mRNA for Met-Ala-Ser (with start codon AUG and stop codon UAA)
  let rnaStr = "AUGGUUUCCUAA"  -- Met-Val-Ser-Stop (AUG GUU UCC UAA)
  putStrLn $ "mRNA sequence: " ++ rnaStr

  case parseRNA rnaStr of
    Nothing -> putStrLn "ERROR: Could not parse RNA sequence."
    Just rna -> do
      putStrLn $ "Parsed RNA:    " ++ showNucleotides rna

      let codons = extractCodons rna
      putStrLn $ "Codons:        " ++ unwords (map showCodon codons)

      case translateFromStart rna of
        Nothing      -> putStrLn "ERROR: No start codon found."
        Just protein -> do
          putStrLn $ "Protein:       " ++ showProtein protein
          putStrLn $ "One-letter:    " ++ map aminoAcidAbbrev1 protein
          putStrLn $ "Length:        " ++ show (length protein) ++ " amino acids"

  putStrLn ""

  -- Full pipeline: DNA -> mRNA -> Protein
  putStrLn "--- Full pipeline: DNA -> mRNA -> Protein ---"
  let fullDNA = "ATGGCTTCTTGGTAA"  -- Met-Ala-Ser-Trp-Stop
  putStrLn $ "DNA:     " ++ fullDNA
  case parseDNA fullDNA >>= transcribeSense of
    Nothing  -> putStrLn "ERROR: Transcription failed."
    Just rna -> do
      putStrLn $ "mRNA:    " ++ showNucleotides rna
      case translateFromStart rna of
        Nothing      -> putStrLn "No start codon found."
        Just protein -> do
          putStrLn $ "Protein: " ++ showProtein protein
          putStrLn $ "1-letter: " ++ map aminoAcidAbbrev1 protein

-- ============================================================
-- Demo 3: Codon Table
-- ============================================================

demoCodonTable :: IO ()
demoCodonTable = do
  divider "DEMO 3: Codon Table Lookup"

  putStrLn "Selected codon lookups:"
  putStrLn ""
  let showLookup c = case codonToAminoAcid c of
        Nothing -> showCodon c ++ " -> (invalid)"
        Just aa -> showCodon c ++ " -> " ++ aminoAcidAbbrev3 aa
                   ++ " (" ++ aminoAcidName aa ++ ")"

  -- Start codon
  putStrLn $ "  " ++ showLookup (A, U, G) ++ "  [START codon]"
  putStrLn ""

  -- Stop codons
  putStrLn "  Stop codons:"
  putStrLn $ "    " ++ showLookup (U, A, A) ++ "  [Ochre]"
  putStrLn $ "    " ++ showLookup (U, A, G) ++ "  [Amber]"
  putStrLn $ "    " ++ showLookup (U, G, A) ++ "  [Opal]"
  putStrLn ""

  -- Demonstrate codon degeneracy (multiple codons -> same amino acid)
  putStrLn "  Codon degeneracy (Leucine has 6 codons):"
  let leuCodons = [(U,U,A),(U,U,G),(C,U,U),(C,U,C),(C,U,A),(C,U,G)]
  mapM_ (\c -> putStrLn $ "    " ++ showLookup c) leuCodons
  putStrLn ""

  -- Serine also has 6 codons (split across two codon families)
  putStrLn "  Serine degeneracy (6 codons, two families):"
  let serCodons = [(U,C,U),(U,C,C),(U,C,A),(U,C,G),(A,G,U),(A,G,C)]
  mapM_ (\c -> putStrLn $ "    " ++ showLookup c) serCodons
  putStrLn ""

  -- Summary statistics
  let entries = codonTableEntries
      stopCount = length (filter (\(_, aa) -> aa == Stop) entries)
      aaCount = length entries - stopCount
  putStrLn $ "  Total codons:  " ++ show (length entries)
  putStrLn $ "  Coding codons: " ++ show aaCount
  putStrLn $ "  Stop codons:   " ++ show stopCount
  putStrLn $ "  Amino acids:   20 (encoded by " ++ show aaCount ++ " codons)"

-- ============================================================
-- Demo 4: Mutation Classification
-- ============================================================

demoMutations :: IO ()
demoMutations = do
  divider "DEMO 4: Mutation Classification"

  putStrLn "Point mutations classified by type-theoretic impact:"
  putStrLn ""

  let showMut name orig mut = do
        let mtype = classifyMutation orig mut
        putStrLn $ "  " ++ name ++ ":"
        putStrLn $ "    Original codon: " ++ showCodon orig
                   ++ " -> " ++ maybe "?" aminoAcidAbbrev3 (codonToAminoAcid orig)
        putStrLn $ "    Mutated codon:  " ++ showCodon mut
                   ++ " -> " ++ maybe "?" aminoAcidAbbrev3 (codonToAminoAcid mut)
        putStrLn $ "    Classification: " ++ show mtype
        putStrLn $ "    Interpretation: " ++ interpretMutation mtype
        putStrLn ""

  -- Silent mutation: CUU -> CUC (both encode Leucine)
  showMut "Silent (synonymous)" (C,U,U) (C,U,C)

  -- Missense mutation: GAG -> GUG (Glu -> Val, as in sickle cell disease)
  showMut "Missense (sickle cell)" (G,A,G) (G,U,G)

  -- Nonsense mutation: CAG -> UAG (Gln -> Stop)
  showMut "Nonsense (premature stop)" (C,A,G) (U,A,G)

  -- Frameshift demonstration
  putStrLn "  Frameshift mutation (insertion):"
  putStrLn "    Original: AUG GCU UCC UAA  (Met-Ala-Ser-Stop)"
  let origRNA = [A,U,G, G,C,U, U,C,C, U,A,A]
  putStrLn $ "    Protein:  " ++ showProtein (translate origRNA)
  putStrLn ""
  -- Insert a U after the first codon: AUG UGC UUC CUA A...
  let shiftedRNA = [A,U,G, U,G,C, U,U,C,C, U,A,A]
  putStrLn "    After inserting U: AUG UGC UUC CUA A..."
  putStrLn $ "    Shifted protein: " ++ showProtein (translate shiftedRNA)
  putStrLn "    Classification: Frameshift"
  putStrLn "    Interpretation: Parse error — all downstream codons corrupted"

interpretMutation :: MutationType -> String
interpretMutation Silent    = "Identity refactoring (no functional change)"
interpretMutation Missense  = "Type-compatible substitution (different amino acid)"
interpretMutation Nonsense  = "Type error (premature termination signal)"
interpretMutation Frameshift = "Parse error (reading frame destroyed)"

-- ============================================================
-- Demo 5: Alternative Splicing
-- ============================================================

demoAlternativeSplicing :: IO ()
demoAlternativeSplicing = do
  divider "DEMO 5: Alternative Splicing"

  putStrLn "Gene: hypothetical EXAMPLE-1 with 4 exons"
  putStrLn ""

  -- Build a gene with 4 exons
  let exon1 = Exon 1 1 12   [A,T,G,G,C,T,A,G,C,T,A,C]  -- ATG GCT AGC TAC
      exon2 = Exon 2 20 31  [G,A,T,C,C,G,A,A,T,G,G,C]  -- GAT CCG AAT GGC
      exon3 = Exon 3 40 51  [T,T,C,A,C,G,G,A,C,C,T,G]  -- TTC ACG GAC CTG
      exon4 = Exon 4 60 71  [A,A,G,T,G,C,T,A,A,T,A,G]  -- AAG TGC TAA TAG

      gene = GeneStructure
        { gsName    = "EXAMPLE-1"
        , gsExons   = [exon1, exon2, exon3, exon4]
        , gsIntrons = [ Intron 1 13 19 [G,T,A,A,G,T,C]
                       , Intron 2 32 39 [G,T,G,A,C,T,A,G]
                       , Intron 3 52 59 [G,T,A,C,G,T,A,G]
                       ]
        }

  putStrLn "  Exon structure:"
  mapM_ (\e -> putStrLn $ "    Exon " ++ show (exonId e)
         ++ ": " ++ showNucleotides (exonSeq e)) (gsExons gene)
  putStrLn ""

  -- Show splicing in different contexts
  let contexts = [DefaultContext, Neural, Muscle, Epithelial, Hepatic]

  mapM_ (\ctx -> do
    let selected = selectExons (gsExons gene) ctx
        matureRNA = concatMap (map dnaToRna . exonSeq) selected
        protein = translate matureRNA
    putStrLn $ "  Context: " ++ show ctx
    putStrLn $ "    Exons included: " ++ show (map exonId selected)
    putStrLn $ "    Mature mRNA:    " ++ showNucleotides matureRNA
    putStrLn $ "    Protein:        " ++ showProtein protein
    putStrLn $ "    1-letter:       " ++ map aminoAcidAbbrev1 protein
    putStrLn ""
    ) contexts

  putStrLn "  Note: One gene produces multiple protein isoforms depending"
  putStrLn "  on cellular context — this is dependent typing in biology."

-- | Convert a DNA nucleotide to its RNA equivalent.
dnaToRna :: Nucleotide -> Nucleotide
dnaToRna T = U
dnaToRna n = n

-- ============================================================
-- Demo 6: ProteinCode construction
-- ============================================================

demoProteinCode :: IO ()
demoProteinCode = do
  divider "DEMO 6: ProteinCode<T> Construction"

  let dnaSeq = [A,T,G,G,C,T,T,C,T,T,G,G,T,A,A]  -- Met-Ala-Ser-Trp-Stop
      exons  = [Exon 1 1 15 dnaSeq]

  case transcribeSense dnaSeq of
    Nothing -> putStrLn "ERROR: Transcription failed."
    Just rna -> do
      let protein = translate rna
          pc = ProteinCode
            { pcGene    = dnaSeq
            , pcProduct = protein
            , pcExons   = exons
            , pcName    = "DEMO-GENE"
            }

      putStrLn $ "Gene name:   " ++ pcName pc
      putStrLn $ "DNA source:  " ++ showNucleotides (pcGene pc)
      putStrLn $ "Exon count:  " ++ show (length (pcExons pc))
      putStrLn $ "Product:     " ++ showProtein (pcProduct pc)
      putStrLn $ "Product (1): " ++ map aminoAcidAbbrev1 (pcProduct pc)
      putStrLn ""
      putStrLn "The ProteinCode type encapsulates the complete compilation"
      putStrLn "pipeline from source (DNA) through to executable (Protein)."

-- ============================================================
-- Main
-- ============================================================

main :: IO ()
main = do
  putStrLn ""
  putStrLn "============================================================"
  putStrLn "  DNA-Lang: Coding Sequences as Executable Functions"
  putStrLn "  A Type-Theoretic Framework for the Central Dogma"
  putStrLn "============================================================"
  putStrLn "  (c) 2026 Matthew Long, YonedaAI Research Collective"
  putStrLn ""

  demoTranscription
  demoTranslation
  demoCodonTable
  demoMutations
  demoAlternativeSplicing
  demoProteinCode

  putStrLn ""
  putStrLn (replicate 60 '=')
  putStrLn "  All demonstrations complete."
  putStrLn (replicate 60 '=')
  putStrLn ""
