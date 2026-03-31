{-
  Main.hs — Demonstration of structural DNA as memory architecture

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Structural DNA as Memory Architecture

  Demonstrates telomere shortening, TAD boundary detection,
  chromosome territory simulation, and memory hierarchy modeling.
-}

module Main where

import Structure
import Telomere
import Centromere
import ChromatinLayout

-- ============================================================
-- Section 1: Telomere Shortening Demonstration
-- ============================================================

-- | Demonstrate linear resource consumption via telomere shortening
demoTelomereShortening :: IO ()
demoTelomereShortening = do
  putStrLn "============================================"
  putStrLn "  TELOMERE SHORTENING (Linear Type Consumption)"
  putStrLn "============================================\n"

  let tel = newTelomere
  putStrLn "Initial telomere state:"
  putStrLn (showTelomere tel)

  -- Perform 10 divisions
  let (tel10, n10) = divideN 10 tel
  putStrLn $ "After 10 divisions (" ++ show n10 ++ " completed):"
  putStrLn (showTelomere tel10)

  -- Perform 50 more divisions (approaching Hayflick limit)
  let (tel60, n60) = divideN 50 tel10
  putStrLn $ "After 50 more divisions (" ++ show n60 ++ " completed):"
  putStrLn (showTelomere tel60)

  -- Simulate full lifespan
  let lifespan = simulateLifespan newTelomere
  putStrLn $ "Full lifespan simulation: " ++ show (length lifespan - 1) ++ " divisions"
  putStrLn $ "Final state: " ++ show (telomereState (snd (last lifespan)))
  putStrLn ""

  -- Demonstrate telomerase rescue
  putStrLn "--- Telomerase-positive cell (stem cell model) ---"
  let stemCell = newTelomereWithLength initialRepeatCount LowTelomerase
  let (stem50, sn50) = divideN 50 stemCell
  putStrLn $ "Stem cell after 50 divisions (" ++ show sn50 ++ " completed):"
  putStrLn (showTelomere stem50)

  -- Demonstrate immortal cell (cancer/germ line)
  putStrLn "--- Telomerase-high cell (cancer model) ---"
  let cancerCell = newTelomereWithLength initialRepeatCount HighTelomerase
  let (cancer100, cn100) = divideN 100 cancerCell
  putStrLn $ "Cancer cell after 100 divisions (" ++ show cn100 ++ " completed):"
  putStrLn (showTelomere cancer100)

-- ============================================================
-- Section 2: Centromere Synchronization Demonstration
-- ============================================================

-- | Demonstrate spindle checkpoint as barrier synchronization
demoCentromereCheckpoint :: IO ()
demoCentromereCheckpoint = do
  putStrLn "============================================"
  putStrLn "  CENTROMERE CHECKPOINT (Barrier Synchronization)"
  putStrLn "============================================\n"

  -- Create centromeres for 3 chromosomes
  let c1 = newCentromere "CEN1" 1
  let c2 = newCentromere "CEN2" 2
  let c3 = newCentromere "CEN3" 3

  putStrLn "Initial state (all unattached):"
  let checkpoint0 = newCheckpoint [c1, c2, c3]
  putStrLn (showCheckpoint checkpoint0)

  -- Attach chromosome 1 to both poles (amphitelic)
  let c1' = attachMicrotubule 'B' (attachMicrotubule 'A' c1)
  putStrLn "After attaching chromosome 1 (amphitelic):"
  putStrLn (showCentromere c1')

  -- Attach chromosome 2
  let c2' = attachMicrotubule 'B' (attachMicrotubule 'A' c2)

  -- Chromosome 3 still unattached - checkpoint should WAIT
  let checkpoint1 = signalCheckpoint c1' (signalCheckpoint c2' checkpoint0)
  let checkpoint1' = evaluateCheckpoint checkpoint1
  putStrLn "Checkpoint with chr3 still unattached:"
  putStrLn (showCheckpoint checkpoint1')

  -- Now attach chromosome 3
  let c3' = attachMicrotubule 'B' (attachMicrotubule 'A' c3)
  let checkpoint2 = signalCheckpoint c3' checkpoint1'
  let checkpoint2' = evaluateCheckpoint checkpoint2
  putStrLn "Checkpoint with all chromosomes attached:"
  putStrLn (showCheckpoint checkpoint2')

  -- Attempt segregation
  case segregate checkpoint2' of
    Nothing -> putStrLn "Segregation BLOCKED (checkpoint not satisfied)\n"
    Just (poleA, poleB) -> do
      putStrLn $ "Segregation ALLOWED: "
        ++ show (length poleA) ++ " chromatids to pole A, "
        ++ show (length poleB) ++ " chromatids to pole B\n"

-- ============================================================
-- Section 3: TAD Boundary Detection
-- ============================================================

-- | Demonstrate TAD boundary detection and insulation
demoTADBoundaries :: IO ()
demoTADBoundaries = do
  putStrLn "============================================"
  putStrLn "  TAD BOUNDARIES (Memory Segmentation)"
  putStrLn "============================================\n"

  -- Create TADs on chromosome 1
  let tad1 = mkTAD "TAD-alpha" 1 0 500000 CompartmentA
  let tad2 = mkTAD "TAD-beta" 1 500000 1200000 CompartmentA
  let tad3 = mkTAD "TAD-gamma" 1 1200000 1800000 CompartmentB

  putStrLn "TAD definitions:"
  putStrLn (showTAD tad1)
  putStrLn (showTAD tad2)
  putStrLn (showTAD tad3)

  -- Check containment
  putStrLn $ "Position 250000 in TAD-alpha: " ++ show (tadContains tad1 250000)
  putStrLn $ "Position 250000 in TAD-beta:  " ++ show (tadContains tad2 250000)
  putStrLn $ "Position 800000 in TAD-beta:  " ++ show (tadContains tad2 800000)
  putStrLn ""

  -- Inter-TAD contact frequencies
  putStrLn "Inter-TAD contact frequencies:"
  putStrLn $ "  alpha-alpha (intra): " ++ show (if True then 1.0 :: Double else 0.0)
  putStrLn $ "  alpha-beta:  " ++ show (interTADContact tad1 tad2)
  putStrLn $ "  alpha-gamma: " ++ show (interTADContact tad1 tad3)
  putStrLn $ "  beta-gamma:  " ++ show (interTADContact tad2 tad3)
  putStrLn ""

  -- Contact matrix
  let cm = buildContactMatrix [tad1, tad2, tad3]
  putStrLn (showContactMatrix cm)

-- ============================================================
-- Section 4: Chromatin Loops as Pointers
-- ============================================================

-- | Demonstrate chromatin loops as pointer dereferencing
demoChomatinLoops :: IO ()
demoChomatinLoops = do
  putStrLn "============================================"
  putStrLn "  CHROMATIN LOOPS (Pointer Dereferencing)"
  putStrLn "============================================\n"

  -- Create enhancer-promoter loop
  let enhancer = LoopAnchor 100000 EnhancerAnchor "SuperEnhancer-1"
  let promoter = LoopAnchor 350000 PromoterAnchor "MYC-promoter"
  let epLoop = mkLoop "SE1-MYC" enhancer promoter True 0.85

  putStrLn "Enhancer-promoter loop:"
  putStrLn (showLoop epLoop)
  putStrLn $ "Is E-P loop: " ++ show (isEnhancerPromoterLoop epLoop)

  case dereferenceLoop epLoop of
    Just (src, tgt) -> putStrLn $ "Dereference: " ++ src ++ " -> " ++ tgt
    Nothing         -> putStrLn "Cannot dereference (not an E-P loop)"
  putStrLn ""

  -- Structural CTCF loop (not an E-P loop)
  let ctcf1 = LoopAnchor 0 CTCFAnchor "CTCF-left"
  let ctcf2 = LoopAnchor 500000 CTCFAnchor "CTCF-right"
  let structLoop = mkLoop "TAD-boundary-loop" ctcf1 ctcf2 True 0.95

  putStrLn "Structural CTCF loop:"
  putStrLn (showLoop structLoop)
  putStrLn $ "Is E-P loop: " ++ show (isEnhancerPromoterLoop structLoop)

  case dereferenceLoop structLoop of
    Just (src, tgt) -> putStrLn $ "Dereference: " ++ src ++ " -> " ++ tgt
    Nothing         -> putStrLn "Cannot dereference (structural loop, not a pointer)"
  putStrLn ""

-- ============================================================
-- Section 5: Chromosome Territory and Memory Hierarchy
-- ============================================================

-- | Demonstrate chromosome territories as memory banks
demoMemoryHierarchy :: IO ()
demoMemoryHierarchy = do
  putStrLn "============================================"
  putStrLn "  MEMORY HIERARCHY (Nuclear Organization)"
  putStrLn "============================================\n"

  -- Create genomic regions
  let activeGene = GenomicRegion "GAPDH" GeneBody 1000 5000 CompartmentA 1
  let enhancer   = GenomicRegion "Enh-1" EnhancerRegion 800 1000 CompartmentA 1
  let silenced   = GenomicRegion "Sat-III" IntergenicRegion 50000 80000 CompartmentB 1
  let lad        = GenomicRegion "LAD-1" LADRegion 90000 120000 CompartmentB 1
  let telomere   = GenomicRegion "Tel-1p" TelomereRegion 0 500 CompartmentB 1

  let allRegions = [activeGene, enhancer, silenced, lad, telomere]

  -- Build structure
  let genome = mkStructure
        (HierarchicalTopology [LinearTopology, LoopedTopology 5])
        allRegions
        [ ("GAPDH", Open)
        , ("Enh-1", Open)
        , ("Sat-III", Closed)
        , ("LAD-1", Closed)
        , ("Tel-1p", Protected)
        ]
        ("Homo sapiens chr1" :: String)

  putStrLn (showStructure genome)

  -- Memory tier assignment
  putStrLn "Memory tier assignments:"
  let amap = accessibility genome
  mapM_ (\r -> do
    let tier = memoryTier r amap
    putStrLn $ "  " ++ regionName r ++ " -> " ++ show tier
    ) allRegions
  putStrLn ""

  -- Compartment map
  let cmap = buildCompartmentMap allRegions
  putStrLn (showCompartmentMap cmap)

  -- Chromosome territories
  let territory1 = mkTerritory 1 (0.3, 0.5, 0.4) 2.0 allRegions
  putStrLn $ "Territory chr" ++ show (territoryChrom territory1)
    ++ ": center=" ++ show (territoryCenter territory1)
    ++ ", radius=" ++ show (territoryRadius territory1)
  putStrLn $ "  Contains GAPDH: " ++ show (territoryContains territory1 activeGene)
  putStrLn $ "  Regions: " ++ show (length (territoryRegions territory1))
  putStrLn ""

  -- Region display
  putStrLn "All regions with details:"
  mapM_ (putStrLn . ("  " ++) . showRegion) allRegions
  putStrLn ""

-- ============================================================
-- Main
-- ============================================================

main :: IO ()
main = do
  putStrLn "======================================================"
  putStrLn "  Structural DNA as Memory Architecture"
  putStrLn "  DNA-Lang Project - Paper 4 Demonstration"
  putStrLn "======================================================\n"

  demoTelomereShortening
  demoCentromereCheckpoint
  demoTADBoundaries
  demoChomatinLoops
  demoMemoryHierarchy

  putStrLn "======================================================"
  putStrLn "  All demonstrations complete."
  putStrLn "======================================================"
