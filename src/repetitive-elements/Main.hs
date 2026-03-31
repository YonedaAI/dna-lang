{-
  Main.hs -- Demonstrations of repetitive element mechanics

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Repetitive Elements as Self-Modifying Code

  Demonstrates:
    1. Cut-and-paste transposition (DNA transposons)
    2. Copy-and-paste retrotransposition (LINEs/SINEs)
    3. Satellite DNA modeling (centromeric/telomeric)
    4. Endogenous retrovirus structure
    5. Transposon domestication (RAG1, syncytin)
    6. Genome composition statistics
-}

module Main where

import Repeat
import Transposon
import SatelliteDNA
import DynamicGenome

main :: IO ()
main = do
  putStrLn "============================================================"
  putStrLn "  DNA-Lang: Repetitive Elements as Self-Modifying Code"
  putStrLn "  Paper 5 of 7"
  putStrLn "============================================================"
  putStrLn ""

  demoCutAndPaste
  demoCopyAndPaste
  demoSatelliteDNA
  demoERV
  demoDomestication
  demoGenomeComposition
  demoTelomereDynamics

-- ─── Demo 1: Cut-and-Paste Transposition ────────────────────────────

demoCutAndPaste :: IO ()
demoCutAndPaste = do
  putStrLn "--- Demo 1: Cut-and-Paste Transposition (DNA Transposons) ---"
  putStrLn ""

  let genome = mkGenome "TestGenome" "AAAAAATCGATCGATCGAAAAAAGGGGGGG"
  putStrLn $ "Original genome:  " ++ showGenome genome
  putStrLn $ "Length:           " ++ show (genomeLength genome) ++ " bp"
  putStrLn ""

  -- Excise a segment (positions 6-17 = "TCGATCGATCG")
  case excise 6 12 genome of
    Nothing -> putStrLn "Excision failed!"
    Just (element, gapped) -> do
      putStrLn $ "Excised element:  " ++ showSequence element
      putStrLn $ "Gapped genome:    " ++ showGenome gapped ++ " -> " ++ showSequence (genomeSequence gapped)

  putStrLn ""

  -- Full cut-and-paste: move element from position 6 to position 20
  case cutAndPaste 6 12 20 genome of
    Nothing -> putStrLn "Cut-and-paste failed!"
    Just result -> do
      putStrLn $ "After cut-paste:  " ++ showSequence (genomeSequence result)
      putStrLn $ "Length preserved: " ++ show (genomeLength result) ++ " bp"

  -- Demonstrate TIR recognition
  let tirL = mkSequence "ACAGTG"
      tirR = reverseComplement tirL
  putStrLn ""
  putStrLn $ "TIR left:         " ++ showSequence tirL
  putStrLn $ "TIR right (RC):   " ++ showSequence tirR
  putStrLn $ "Valid TIR pair:   " ++ show (isTIR tirL tirR)

  -- Demonstrate TSD insertion
  putStrLn ""
  let element = mkSequence "NNNNNNNNNN"  -- simplified transposon
  case insertWithTSD 10 5 element genome of
    Nothing -> putStrLn "TSD insertion failed!"
    Just result -> do
      putStrLn $ "After TSD insert: " ++ showSequence (genomeSequence result)
      putStrLn $ "New length:       " ++ show (genomeLength result) ++ " bp (grew by TSD)"

  putStrLn ""
  putStrLn ""

-- ─── Demo 2: Copy-and-Paste Retrotransposition ─────────────────────

demoCopyAndPaste :: IO ()
demoCopyAndPaste = do
  putStrLn "--- Demo 2: Copy-and-Paste Retrotransposition (LINEs) ---"
  putStrLn ""

  let genome = mkGenome "RetroGenome" "AAAAATCGATCGAAAAAGGGGG"
  putStrLn $ "Original genome:  " ++ showGenome genome
  putStrLn $ "Original length:  " ++ show (genomeLength genome) ++ " bp"
  putStrLn ""

  -- Copy-and-paste: copy from position 5, length 8, insert at position 17
  case copyAndPaste 5 8 17 genome of
    Nothing -> putStrLn "Copy-and-paste failed!"
    Just result -> do
      putStrLn $ "After copy-paste: " ++ showSequence (genomeSequence result)
      putStrLn $ "New length:       " ++ show (genomeLength result) ++ " bp (grew by element size)"
      putStrLn $ "Growth:           +" ++ show (genomeLength result - genomeLength genome) ++ " bp"

  putStrLn ""

  -- Demonstrate L1-style TPRT
  putStrLn "Target-Primed Reverse Transcription (TPRT):"
  case targetPrimedRT 5 8 17 genome of
    Nothing -> putStrLn "  TPRT failed!"
    Just result -> do
      putStrLn $ "  After TPRT:     " ++ showSequence (genomeSequence result)
      putStrLn $ "  New length:     " ++ show (genomeLength result) ++ " bp (includes TSD)"

  -- LINE vs SINE autonomy
  putStrLn ""
  let l1 = Retrotransposon LINE (mkSequence "ATCG") (Just (Gene "RT" (mkSequence "ATG"))) True Active
      alu = Retrotransposon SINE (mkSequence "GCGC") Nothing True Active
  putStrLn $ "L1 (LINE):  autonomous=" ++ show (isAutonomous l1) ++ ", mobile=" ++ show (isMobile l1)
  putStrLn $ "Alu (SINE): autonomous=" ++ show (isAutonomous alu) ++ ", mobile=" ++ show (isMobile alu)
  putStrLn "  (Alu depends on L1 machinery for retrotransposition)"

  putStrLn ""
  putStrLn ""

-- ─── Demo 3: Satellite DNA ─────────────────────────────────────────

demoSatelliteDNA :: IO ()
demoSatelliteDNA = do
  putStrLn "--- Demo 3: Satellite DNA as Memory Alignment ---"
  putStrLn ""

  -- Alpha satellite (centromeric)
  let alphaSat = mkAlphaSatellite 10
  putStrLn $ "Alpha satellite:  " ++ showSatellite alphaSat
  putStrLn $ "  Classification: " ++ maybe "N/A" show (satelliteClass alphaSat)
  putStrLn $ "  Array preview:  " ++ showRepeatArray 60 alphaSat
  putStrLn ""

  -- Telomeric repeat
  let telRepeat = mkTelomericRepeat 500
  putStrLn $ "Telomeric repeat: " ++ showSatellite telRepeat
  putStrLn $ "  Classification: " ++ maybe "N/A" show (satelliteClass telRepeat)
  putStrLn $ "  Telomere len:   " ++ maybe "N/A" (\n -> show n ++ " bp") (telomereLength telRepeat)
  putStrLn $ "  Array preview:  " ++ showRepeatArray 60 telRepeat
  putStrLn ""

  -- Microsatellite (CA repeat - common STR)
  let caRepeat = mkMicrosatellite "CA" 20
  putStrLn $ "Microsatellite:   " ++ showSatellite caRepeat
  putStrLn $ "  Array:          " ++ showRepeatArray 60 caRepeat
  putStrLn ""

  -- Minisatellite
  let miniSat = mkMinisatellite "AATGGAATGGAATGG" 15 Subtelomeric
  putStrLn $ "Minisatellite:    " ++ showSatellite miniSat
  putStrLn $ "  Aligned to 15:  " ++ show (isAligned 15 miniSat)
  putStrLn ""

  -- Expansion and contraction
  let expanded = expandRepeat 5 caRepeat
  let contracted = contractRepeat 10 caRepeat
  putStrLn $ "CA repeat expanded (+5):   " ++ showSatellite expanded
  putStrLn $ "CA repeat contracted (-10): " ++ showSatellite contracted

  putStrLn ""
  putStrLn ""

-- ─── Demo 4: Endogenous Retroviruses ───────────────────────────────

demoERV :: IO ()
demoERV = do
  putStrLn "--- Demo 4: Endogenous Retroviruses as Legacy Imports ---"
  putStrLn ""

  -- Intact ERV
  let intactERV = ERV
        { ervLtrLeft  = mkSequence (take 50 (cycle "ATCGATCG"))
        , ervLtrRight = mkSequence (take 50 (cycle "ATCGATCG"))
        , ervGag      = Just (Gene "gag" (mkSequence (replicate 400 'A')))
        , ervPol      = Just (Gene "pol" (mkSequence (replicate 600 'A')))
        , ervEnv      = Just (Gene "env" (mkSequence (replicate 500 'A')))
        , ervIntact   = True
        , ervActivity = Suppressed
        }
  putStrLn $ "Intact ERV:       " ++ repeatSummary intactERV
  putStrLn $ "  Length:          " ++ show (repeatLength intactERV) ++ " bp"
  putStrLn $ "  Active:          " ++ show (isActive intactERV)
  putStrLn $ "  Has gag/pol/env: True/True/True"
  putStrLn ""

  -- Degraded ERV (lost env)
  let degradedERV = intactERV
        { ervEnv     = Nothing
        , ervIntact  = False
        , ervActivity = Fossilized
        }
  putStrLn $ "Degraded ERV:     " ++ repeatSummary degradedERV
  putStrLn $ "  Length:          " ++ show (repeatLength degradedERV) ++ " bp"
  putStrLn $ "  Has gag/pol/env: True/True/False"
  putStrLn ""

  -- Solo LTR (all internal genes lost)
  let soloLTR = ERV
        { ervLtrLeft  = mkSequence (take 50 (cycle "ATCGATCG"))
        , ervLtrRight = mkSequence ""
        , ervGag      = Nothing
        , ervPol      = Nothing
        , ervEnv      = Nothing
        , ervIntact   = False
        , ervActivity = Fossilized
        }
  putStrLn $ "Solo LTR:         " ++ repeatSummary soloLTR
  putStrLn $ "  Length:          " ++ show (repeatLength soloLTR) ++ " bp"
  putStrLn $ "  (Retains promoter/enhancer sequences from original LTR)"
  putStrLn ""

  -- ERV decay progression
  putStrLn "ERV Decay Progression (Legacy Import Degradation):"
  putStrLn "  1. Intact provirus:  gag-pol-env flanked by LTRs (suppressed)"
  putStrLn "  2. Gene degradation: accumulating stop codons, deletions"
  putStrLn "  3. Solo LTR:         recombination deletes internal genes"
  putStrLn "  4. Solo LTR serves as orphaned regulatory element"

  putStrLn ""
  putStrLn ""

-- ─── Demo 5: Transposon Domestication ──────────────────────────────

demoDomestication :: IO ()
demoDomestication = do
  putStrLn "--- Demo 5: Transposon Domestication (Type Naturalization) ---"
  putStrLn ""

  -- RAG1: domesticated Transib transposase
  let transib = DNATransposon
        { dtTirLeft     = mkSequence "CACAGTG"
        , dtTirRight    = reverseComplement (mkSequence "CACAGTG")
        , dtBody        = mkSequence (take 200 (cycle "ATCGATCG"))
        , dtTransposase = Gene "Transib_transposase" (mkSequence (replicate 100 'A'))
        , dtActivity    = Active
        }

  putStrLn "Example 1: RAG1 - Domesticated Transib Transposase"
  putStrLn $ "  Before: " ++ repeatSummary transib
  putStrLn $ "    Mobile:      " ++ show (isMobile transib)

  let rag1 = domesticate transib "V(D)J recombination"
  putStrLn $ "  After:  " ++ repeatSummary rag1
  putStrLn $ "    Mobile:      " ++ show (isMobile rag1)
  putStrLn $ "    Activity:    " ++ activityLabel rag1
  putStrLn "  Function: Catalyzes V(D)J recombination in adaptive immunity"
  putStrLn "  Evidence: DDE catalytic triad, RSS recognition (TIR-like)"
  putStrLn ""

  -- Syncytin: domesticated ERV env protein
  let hervW = ERV
        { ervLtrLeft  = mkSequence (take 40 (cycle "ATCGATCG"))
        , ervLtrRight = mkSequence (take 40 (cycle "ATCGATCG"))
        , ervGag      = Nothing
        , ervPol      = Nothing
        , ervEnv      = Just (Gene "env_W" (mkSequence (replicate 200 'A')))
        , ervIntact   = False
        , ervActivity = Active
        }

  putStrLn "Example 2: Syncytin - Domesticated ERV Envelope Protein"
  putStrLn $ "  Before: " ++ repeatSummary hervW
  let syncytin = domesticate hervW "placental cell fusion (syncytin)"
  putStrLn $ "  After:  " ++ repeatSummary syncytin
  putStrLn $ "    Activity:    " ++ activityLabel syncytin
  putStrLn "  Function: Mediates trophoblast fusion in placenta formation"
  putStrLn "  Evidence: Fusogenic env protein, essential for placentation"
  putStrLn "  Note: Independently domesticated 10+ times across mammals"
  putStrLn ""

  -- CENPB: domesticated pogo transposase
  let pogo = DNATransposon
        { dtTirLeft     = mkSequence "TTCGTTGG"
        , dtTirRight    = reverseComplement (mkSequence "TTCGTTGG")
        , dtBody        = mkSequence (take 150 (cycle "ATCGATCG"))
        , dtTransposase = Gene "pogo_transposase" (mkSequence (replicate 80 'A'))
        , dtActivity    = Active
        }

  putStrLn "Example 3: CENP-B - Domesticated pogo Transposase"
  let cenpb = domesticate pogo "centromere function (CENP-B)"
  putStrLn $ "  Before: " ++ repeatSummary pogo
  putStrLn $ "  After:  " ++ repeatSummary cenpb
  putStrLn "  Function: Binds CENP-B box in alpha-satellite, centromere organization"

  putStrLn ""
  putStrLn ""

-- ─── Demo 6: Genome Composition ────────────────────────────────────

demoGenomeComposition :: IO ()
demoGenomeComposition = do
  putStrLn "--- Demo 6: Genome Composition Statistics ---"
  putStrLn ""

  let dg = mkExampleGenome

  putStrLn $ "Genome: " ++ show (dgGenome dg)
  putStrLn ""

  -- Show tracked elements
  putStrLn "Tracked elements:"
  mapM_ (\rl -> putStrLn $ "  " ++ rlName rl ++ " @ " ++ show (rlPosition rl)
                         ++ ": " ++ repeatSummary (rlElement rl))
        (dgRepeats dg)
  putStrLn ""

  -- Show composition
  let comp = computeComposition dg
  putStr (showComposition comp)
  putStrLn ""

  -- Element counts
  let (dnaT, retro, sat, erv) = elementCount dg
  putStrLn $ "Element counts: DNA transposons=" ++ show dnaT
           ++ ", Retrotransposons=" ++ show retro
           ++ ", Satellites=" ++ show sat
           ++ ", ERVs=" ++ show erv
  putStrLn ""

  -- Show domesticated elements
  let dom = listDomesticated dg
  putStrLn "Domesticated elements:"
  mapM_ (\(n, f) -> putStrLn $ "  " ++ n ++ " -> " ++ f) dom

  putStrLn ""

  -- Simulate transposition events
  putStrLn "Simulating transposition events..."
  let dg2 = simulateCopyPaste "L1_001" 400 "L1_002" dg
      dg3 = simulateCutPaste "Tc1" 450 dg2
      dg4 = domesticateElement "HERV_K1" "immune regulation" dg3

  putStrLn ""
  putStr (showEventLog dg4)

  putStrLn ""
  let comp2 = computeComposition dg4
  putStr (showComposition comp2)

  putStrLn ""
  putStrLn ""

-- ─── Demo 7: Telomere Dynamics ─────────────────────────────────────

demoTelomereDynamics :: IO ()
demoTelomereDynamics = do
  putStrLn "--- Demo 7: Telomere Dynamics (Sentinel Value Maintenance) ---"
  putStrLn ""

  let tel = mkTelomericRepeat 1667  -- ~10,000 bp (newborn human telomere)
  putStrLn $ "Newborn telomere: " ++ showSatellite tel
  putStrLn $ "  Length:         " ++ maybe "N/A" (\n -> show n ++ " bp") (telomereLength tel)
  putStrLn ""

  -- Simulate aging (50 cell divisions, losing ~20 repeats each)
  putStrLn "Simulating cellular aging (telomere shortening):"
  let divisions = [10, 20, 30, 40, 50 :: Int]
  mapM_ (\d ->
    let shortened = iterate (telomereShorten 20) tel !! d
        len = maybe 0 id (telomereLength shortened)
    in putStrLn $ "  After " ++ show d ++ " divisions: "
                ++ maybe "N/A" (\n -> show n ++ " bp") (telomereLength shortened)
                ++ if len < 3000 then " [CRITICAL - senescence threshold]" else ""
    ) divisions

  putStrLn ""

  -- Telomerase extension (stem cells / cancer)
  let shortened50 = iterate (telomereShorten 20) tel !! 50
  putStrLn "Telomerase activation (stem cells / cancer):"
  let restored = telomereExtend 500 shortened50
  putStrLn $ "  Before:  " ++ maybe "N/A" (\n -> show n ++ " bp") (telomereLength shortened50)
  putStrLn $ "  After:   " ++ maybe "N/A" (\n -> show n ++ " bp") (telomereLength restored)
  putStrLn "  (Telomerase extends telomeres, enabling continued division)"

  putStrLn ""
  putStrLn "============================================================"
  putStrLn "  All demonstrations complete."
  putStrLn "============================================================"
