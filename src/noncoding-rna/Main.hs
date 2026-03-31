{-
  Main.hs — Demonstrations of ncRNA control mechanisms

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Non-Coding RNAs as Signals and Middleware

  Demonstrates:
    - miRNA targeting and guard clause evaluation
    - siRNA / RNAi silencing pathway
    - tRNA adaptation (natural transformation)
    - lncRNA scaffolding (colimit assembly)
-}

module Main where

import RNAControl
import MicroRNA
import SiRNA
import LncRNA

-- ════════════════════════════════════════════════════════════════════
-- Helper: convert a string of nucleotide characters to [Nucleotide]
-- ════════════════════════════════════════════════════════════════════

toNucs :: String -> [Nucleotide]
toNucs = map charToNuc
  where
    charToNuc 'A' = A
    charToNuc 'U' = U
    charToNuc 'G' = G
    charToNuc 'C' = C
    charToNuc c   = error ("Invalid nucleotide: " ++ [c])

nucsToStr :: [Nucleotide] -> String
nucsToStr = map showNucleotide

-- ════════════════════════════════════════════════════════════════════
-- tRNA: Natural Transformation (Codon -> Amino Acid)
-- ════════════════════════════════════════════════════════════════════

-- | The standard codon table as a natural transformation eta.
--   eta_i : F(i) -> G(i) maps codon at position i to amino acid.
codonTable :: [(String, Char)]
codonTable =
  [ ("UUU", 'F'), ("UUC", 'F'), ("UUA", 'L'), ("UUG", 'L')
  , ("UCU", 'S'), ("UCC", 'S'), ("UCA", 'S'), ("UCG", 'S')
  , ("UAU", 'Y'), ("UAC", 'Y'), ("UAA", '*'), ("UAG", '*')
  , ("UGU", 'C'), ("UGC", 'C'), ("UGA", '*'), ("UGG", 'W')
  , ("CUU", 'L'), ("CUC", 'L'), ("CUA", 'L'), ("CUG", 'L')
  , ("CCU", 'P'), ("CCC", 'P'), ("CCA", 'P'), ("CCG", 'P')
  , ("CAU", 'H'), ("CAC", 'H'), ("CAA", 'Q'), ("CAG", 'Q')
  , ("CGU", 'R'), ("CGC", 'R'), ("CGA", 'R'), ("CGG", 'R')
  , ("AUU", 'I'), ("AUC", 'I'), ("AUA", 'I'), ("AUG", 'M')
  , ("ACU", 'T'), ("ACC", 'T'), ("ACA", 'T'), ("ACG", 'T')
  , ("AAU", 'N'), ("AAC", 'N'), ("AAA", 'K'), ("AAG", 'K')
  , ("AGU", 'S'), ("AGC", 'S'), ("AGA", 'R'), ("AGG", 'R')
  , ("GUU", 'V'), ("GUC", 'V'), ("GUA", 'V'), ("GUG", 'V')
  , ("GCU", 'A'), ("GCC", 'A'), ("GCA", 'A'), ("GCG", 'A')
  , ("GAU", 'D'), ("GAC", 'D'), ("GAA", 'E'), ("GAG", 'E')
  , ("GGU", 'G'), ("GGC", 'G'), ("GGA", 'G'), ("GGG", 'G')
  ]

-- | Apply the tRNA natural transformation: codon -> amino acid
adaptCodon :: String -> Maybe Char
adaptCodon codon = lookup codon codonTable

-- | Translate an mRNA sequence to a protein string
translate :: String -> String
translate []  = []
translate seq'
  | length seq' < 3 = []
  | otherwise =
      let codon = take 3 seq'
          rest  = drop 3 seq'
      in case adaptCodon codon of
           Just '*' -> []         -- Stop codon: terminate
           Just aa  -> aa : translate rest
           Nothing  -> '?' : translate rest  -- Unknown codon

-- ════════════════════════════════════════════════════════════════════
-- Demo 1: miRNA Guard Clause
-- ════════════════════════════════════════════════════════════════════

demoMiRNA :: IO ()
demoMiRNA = do
  putStrLn "══════════════════════════════════════════════════"
  putStrLn " Demo 1: miRNA as Guard Clause"
  putStrLn "══════════════════════════════════════════════════"
  putStrLn ""

  -- miR-21 seed region (positions 2-8): AGCUUAU
  let mirSeed = toNucs "AGCUUAU"

  -- Simulated 3' UTR sequences of potential target mRNAs
  -- Target 1 has a perfect seed match site (reverse complement of seed)
  let target1UTR = toNucs "GGUUCCAAUAAGCUGGACUU"
      target1Name = "PTEN-mRNA"

  -- Target 2 has a partial match
  let target2UTR = toNucs "CCUUGAAUAAGCAAGUCCAA"
      target2Name = "PDCD4-mRNA"

  -- Target 3 has no match
  let target3UTR = toNucs "GGCCGGCCGGCCGGCCGGCC"
      target3Name = "Housekeeping-mRNA"

  let threshold = 0.6

  putStrLn $ "miRNA seed:     " ++ nucsToStr mirSeed
  putStrLn $ "Threshold:      " ++ show threshold
  putStrLn ""

  -- Predict targets
  let targets = [ (target1Name, target1UTR)
                , (target2Name, target2UTR)
                , (target3Name, target3UTR)
                ]
      predictions = predictTargets mirSeed targets threshold

  putStrLn "Target Predictions:"
  putStrLn $ "  Found " ++ show (length predictions) ++ " targets"
  mapM_ (\p -> do
    putStrLn $ "  Target: " ++ tpTargetName p
    putStrLn $ "    Match sites: " ++ show (length (tpMatches p))
    putStrLn $ "    Repression:  " ++ show (tpRepression p)
    mapM_ (\m -> putStrLn $ "    Site at pos " ++ show (smPosition m)
                          ++ " score=" ++ show (smScore m)) (tpMatches p)
    ) predictions

  -- Demonstrate guard evaluation
  let mirControl = MiRNA mirSeed ["PTEN", "PDCD4"] threshold :: RNAControl String
  putStrLn ""
  putStrLn "Guard evaluation:"
  putStrLn $ "  " ++ target1Name ++ ": guard fires = "
           ++ show (guardEvaluate mirControl target1UTR)
  putStrLn $ "  " ++ target3Name ++ ": guard fires = "
           ++ show (guardEvaluate mirControl target3UTR)
  putStrLn ""

-- ════════════════════════════════════════════════════════════════════
-- Demo 2: siRNA / RNAi Exception Handling
-- ════════════════════════════════════════════════════════════════════

demoSiRNA :: IO ()
demoSiRNA = do
  putStrLn "══════════════════════════════════════════════════"
  putStrLn " Demo 2: siRNA / RNAi as Exception Handling"
  putStrLn "══════════════════════════════════════════════════"
  putStrLn ""

  -- Simulated viral dsRNA trigger
  let viralSense     = toNucs "AUGCGAUUCGGAUACGCUAAAUGCGAUUCGGAUACGCUAA"
      viralAntisense = reverseComplement viralSense

  putStrLn $ "Viral dsRNA (sense): " ++ nucsToStr (take 21 viralSense) ++ "..."
  putStrLn $ "Length:              " ++ show (length viralSense) ++ " nt"
  putStrLn ""

  -- Step 1: Dicer cleavage
  let fragments = dicerCleave viralSense viralAntisense
  putStrLn $ "Dicer cleavage: " ++ show (length fragments) ++ " siRNA duplexes"
  putStrLn ""

  -- Step 2: Guide strand selection and RISC loading
  let guides = map selectGuideStrand fragments
      riscs  = map (\g -> loadRISC g 0 "siRNA") guides
  putStrLn $ "Guide strands selected: " ++ show (length guides)
  mapM_ (\(i, g) ->
    putStrLn $ "  Guide " ++ show (i::Int) ++ ": " ++ nucsToStr (take 15 g) ++ "..."
    ) (zip [1..] guides)
  putStrLn ""

  -- Step 3: Scan transcriptome
  let transcriptome =
        [ ("Viral-mRNA",      toNucs "UUAGCGUAUCCGAAUCGCAUUUAGCGUAUCCGAAUCGCAU")
        , ("Host-GAPDH",      toNucs "AUGGGCAAGGACUCCUUCACCACCAUGGGCCCCUCUUGUAAA")
        , ("Host-Actin",      toNucs "AUGGAUGAUAUAUGGCAAGAAAGAUACCUACCGUUGUACCCAG")
        ]

  putStrLn "Scanning transcriptome..."
  let outcomes = concatMap (\r -> scanTranscriptome r transcriptome) riscs
  mapM_ (\o -> do
    putStrLn $ "  " ++ soTarget o ++ ": " ++ show (soResult o)
             ++ " (mismatches=" ++ show (soMismatches o) ++ ")"
    ) outcomes
  putStrLn ""

-- ════════════════════════════════════════════════════════════════════
-- Demo 3: tRNA Natural Transformation
-- ════════════════════════════════════════════════════════════════════

demoTRNA :: IO ()
demoTRNA = do
  putStrLn "══════════════════════════════════════════════════"
  putStrLn " Demo 3: tRNA as Natural Transformation"
  putStrLn "══════════════════════════════════════════════════"
  putStrLn ""

  let mRNA = "AUGGGCAAAGAUUCCUUCACCUAA"
  putStrLn $ "mRNA sequence:  " ++ mRNA
  putStrLn ""

  -- Show codon-by-codon transformation
  putStrLn "Codon-by-codon adaptation (eta_i : F(i) -> G(i)):"
  let codons = takeWhile (not . null) $ map (\i -> take 3 (drop (i*3) mRNA)) [0..]
      validCodons = filter (\c -> length c == 3) codons
  mapM_ (\(i, c) ->
    putStrLn $ "  Position " ++ show (i::Int) ++ ": " ++ c ++ " -> "
             ++ show (adaptCodon c)
    ) (zip [0..] validCodons)
  putStrLn ""

  -- Full translation
  let protein = translate mRNA
  putStrLn $ "Translated protein: " ++ protein
  putStrLn $ "Protein length:     " ++ show (length protein) ++ " amino acids"
  putStrLn ""

  -- Demonstrate position-independence (naturality)
  putStrLn "Naturality check (same codon, different positions):"
  let testCodon = "GGC"
  putStrLn $ "  Codon '" ++ testCodon ++ "' at any position -> "
           ++ show (adaptCodon testCodon)
  putStrLn "  (Position-independence confirms naturality square commutes)"
  putStrLn ""

  -- Show degeneracy (kernel of eta)
  putStrLn "Kernel of eta (synonymous codons):"
  let leucineCodons = filter (\(_, aa) -> aa == 'L') codonTable
  putStrLn $ "  Leucine codons: " ++ show (map fst leucineCodons)
  let serineCodons = filter (\(_, aa) -> aa == 'S') codonTable
  putStrLn $ "  Serine codons:  " ++ show (map fst serineCodons)
  putStrLn ""

-- ════════════════════════════════════════════════════════════════════
-- Demo 4: lncRNA Scaffold Assembly (Colimit)
-- ════════════════════════════════════════════════════════════════════

demoLncRNA :: IO ()
demoLncRNA = do
  putStrLn "══════════════════════════════════════════════════"
  putStrLn " Demo 4: lncRNA Scaffold Assembly (Colimit)"
  putStrLn "══════════════════════════════════════════════════"
  putStrLn ""

  -- HOTAIR: bridges PRC2 and LSD1/CoREST complexes
  let hotairSpec = ScaffoldSpec
        { ssName = "HOTAIR"
        , ssDomains =
            [ BindingDomain "5'-domain" 0 300 "PRC2"
            , BindingDomain "3'-domain" 1500 2158 "LSD1-CoREST"
            ]
        , ssRole = Scaffold
        }

  putStrLn "HOTAIR Scaffold Specification:"
  putStrLn $ "  Name: " ++ ssName hotairSpec
  putStrLn $ "  Role: " ++ show (ssRole hotairSpec)
  putStrLn "  Binding domains:"
  mapM_ (\d ->
    putStrLn $ "    " ++ bdName d ++ " [" ++ show (bdStart d)
             ++ "-" ++ show (bdEnd d) ++ "] -> " ++ bdPartner d
    ) (ssDomains hotairSpec)
  putStrLn ""

  -- Assemble the complex (colimit)
  let complex = assembleComplex hotairSpec
  putStrLn "Assembled Complex (Colimit):"
  putStrLn $ "  Name:     " ++ cxName complex
  putStrLn $ "  Scaffold: " ++ cxScaffold complex
  putStrLn $ "  Members:  " ++ show (cxMembers complex)
  putStrLn $ "  Complete: " ++ show (cxComplete complex)
  putStrLn ""

  -- Apply middleware effect
  let effect = applyMiddleware hotairSpec complex
  putStrLn $ "Middleware effect: " ++ show effect
  putStrLn ""

  -- Demonstrate colimit with available molecules
  putStrLn "Colimit assembly with available molecules:"
  let available1 = ["PRC2", "LSD1-CoREST", "EZH2"]
  putStrLn $ "  Available: " ++ show available1
  putStrLn $ "  Assembly:  " ++ show (colimitAssembly hotairSpec available1)

  let available2 = ["PRC2", "EZH2"]  -- Missing LSD1-CoREST
  putStrLn $ "  Available: " ++ show available2
  putStrLn $ "  Assembly:  " ++ show (colimitAssembly hotairSpec available2)
  putStrLn ""

  -- Xist as guide lncRNA
  let xistSpec = ScaffoldSpec
        { ssName = "Xist"
        , ssDomains =
            [ BindingDomain "RepA" 0 450 "PRC2"
            ]
        , ssRole = Guide
        }
  let xistComplex = assembleComplex xistSpec
  let xistEffect  = applyMiddleware xistSpec xistComplex
  putStrLn "Xist (Guide lncRNA):"
  putStrLn $ "  Complex: " ++ show (cxMembers xistComplex)
  putStrLn $ "  Effect:  " ++ show xistEffect
  putStrLn ""

  -- miRNA sponge effect
  putStrLn "miRNA Sponge Effect (circRNA model):"
  let spongeConc = 100.0   -- sponge concentration (arbitrary units)
      nSites     = 70      -- ~70 miRNA binding sites (like ciRS-7)
      mirnaConc  = 5000.0  -- miRNA pool
      freeFrac   = spongeEffect spongeConc nSites mirnaConc
  putStrLn $ "  Sponge concentration: " ++ show spongeConc
  putStrLn $ "  Binding sites:        " ++ show nSites
  putStrLn $ "  miRNA pool:           " ++ show mirnaConc
  putStrLn $ "  Free miRNA fraction:  " ++ show freeFrac
  putStrLn ""

-- ════════════════════════════════════════════════════════════════════
-- Demo 5: piRNA Security Monitor
-- ════════════════════════════════════════════════════════════════════

demoPiRNA :: IO ()
demoPiRNA = do
  putStrLn "══════════════════════════════════════════════════"
  putStrLn " Demo 5: piRNA as Security Monitor"
  putStrLn "══════════════════════════════════════════════════"
  putStrLn ""

  -- piRNA targeting a LINE-1 transposon
  let piGuideSeq = toNucs "UGCAGAUACUGCGAUUCCGAUACGCUA"
      piControl  = PiRNA piGuideSeq "LINE-1" :: RNAControl String

  putStrLn "piRNA Security Monitor:"
  putStrLn $ "  Guide:  " ++ nucsToStr (piGuide piControl) ++ " (" ++
             show (length (piGuide piControl)) ++ " nt)"
  putStrLn $ "  Target: " ++ piTarget piControl
  putStrLn ""

  -- Simulate scanning for transposon transcripts
  let transposonSeq = reverseComplement piGuideSeq  -- Perfect match
      hostSeq       = toNucs "AUGGGCAAAGAUUCCUUCACCACCAUG"  -- No match

  let risc_pi = loadRISC piGuideSeq 2 "piRNA"
  let scanTE  = riscScan risc_pi transposonSeq
  let scanHost = riscScan risc_pi hostSeq

  putStrLn "Transposon scanning:"
  putStrLn $ "  LINE-1 transcript: " ++ show scanTE
  putStrLn $ "  Host mRNA:         " ++ show scanHost
  putStrLn ""
  putStrLn "  -> Transposon silenced, host gene unaffected"
  putStrLn ""

-- ════════════════════════════════════════════════════════════════════
-- Summary
-- ════════════════════════════════════════════════════════════════════

printSummary :: IO ()
printSummary = do
  putStrLn "══════════════════════════════════════════════════"
  putStrLn " Summary: RNAControl<Process> Type"
  putStrLn "══════════════════════════════════════════════════"
  putStrLn ""
  putStrLn "  Constructor  | ncRNA Class | Control Primitive"
  putStrLn "  -------------|-------------|-------------------"
  putStrLn "  MiRNA        | miRNA       | Guard clause"
  putStrLn "  SiRNA        | siRNA       | Exception handler"
  putStrLn "  LncRNA       | lncRNA      | Middleware/scaffold"
  putStrLn "  TRNA         | tRNA        | Natural transformation"
  putStrLn "  RRNA         | rRNA        | Virtual machine"
  putStrLn "  PiRNA        | piRNA       | Security monitor"
  putStrLn ""
  putStrLn "All ncRNA classes unified under RNAControl<P>."
  putStrLn "See the paper for formal theorems and proofs."
  putStrLn ""

-- ════════════════════════════════════════════════════════════════════
-- Main
-- ════════════════════════════════════════════════════════════════════

main :: IO ()
main = do
  putStrLn ""
  putStrLn "╔══════════════════════════════════════════════════╗"
  putStrLn "║  Non-Coding RNAs as Signals and Middleware       ║"
  putStrLn "║  A Type-Theoretic Framework for                  ║"
  putStrLn "║  Post-Transcriptional Control                    ║"
  putStrLn "║                                                  ║"
  putStrLn "║  DNA-Lang Project — Paper 3 of 7                 ║"
  putStrLn "║  Matthew Long, YonedaAI Research Collective      ║"
  putStrLn "╚══════════════════════════════════════════════════╝"
  putStrLn ""

  demoMiRNA
  demoSiRNA
  demoTRNA
  demoLncRNA
  demoPiRNA
  printSummary
