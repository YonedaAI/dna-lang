{-
  Main.hs -- Demonstration of epigenetic marks as runtime state

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Epigenetic Marks as Runtime State

  Demonstrates:
    1. Methylation state changes (DNMT3, TET, DNMT1 maintenance)
    2. Histone modification cascades (writer/eraser enzymes)
    3. Bivalent chromatin resolution (stem cell differentiation)
    4. Epigenetic inheritance simulation (cell division)
    5. X-inactivation as process isolation
    6. Chromatin remodeling as garbage collection
    7. Methylation monad composition
-}

module Main where

import qualified Methylation as M
import qualified HistoneModification as H
import qualified Accessibility as A
import State

-- ============================================================
-- Demo 1: Methylation state changes
-- ============================================================

demo1_methylation :: IO ()
demo1_methylation = do
  putStrLn "============================================"
  putStrLn "Demo 1: DNA Methylation as Configuration Flags"
  putStrLn "============================================"
  putStrLn ""

  -- Create a CpG island with unmethylated sites (active promoter)
  let cpgSites = [ M.CpGSite pos M.Unmethylated | pos <- [100, 110 .. 300] ]
      initialMap = M.MethylationMap cpgSites

  putStrLn $ "Initial CpG island: " ++ M.showMethylationMap initialMap
  putStrLn $ "  Methylation fraction: " ++ show (M.methylationFraction initialMap)
  putStrLn ""

  -- Apply DNMT3 (de novo methylation) to silence the gene
  let targetPositions = [100, 120, 140, 160, 180, 200]
      afterDNMT3 = M.applyDNMT3 targetPositions initialMap

  putStrLn "After DNMT3 (de novo methylation):"
  putStrLn $ "  " ++ M.showMethylationMap afterDNMT3
  putStrLn $ "  Methylation fraction: " ++ show (M.methylationFraction afterDNMT3)
  putStrLn ""

  -- Apply TET (active demethylation) to reactivate
  let tetTargets = [100, 120, 140]
      afterTET = M.applyTET tetTargets afterDNMT3

  putStrLn "After TET (oxidation of 5mC -> 5hmC):"
  putStrLn $ "  " ++ M.showMethylationMap afterTET
  putStrLn $ "  Site 100 status: " ++ M.showMethylationStatus (M.getSiteStatus 100 afterTET)
  putStrLn $ "  Site 200 status: " ++ M.showMethylationStatus (M.getSiteStatus 200 afterTET)
  putStrLn ""

  -- Simulate replication + maintenance methylation
  let afterDivision = M.maintenanceMethylation afterDNMT3

  putStrLn "After replication + DNMT1 maintenance:"
  putStrLn $ "  " ++ M.showMethylationMap afterDivision
  putStrLn $ "  Methylation fraction: " ++ show (M.methylationFraction afterDivision)
  putStrLn "  (DNMT1 restores hemimethylated -> fully methylated)"
  putStrLn ""

-- ============================================================
-- Demo 2: Histone Modification Cascades
-- ============================================================

demo2_histoneModifications :: IO ()
demo2_histoneModifications = do
  putStrLn "============================================"
  putStrLn "Demo 2: Histone Modifications as Permissions"
  putStrLn "============================================"
  putStrLn ""

  -- Start with unmarked nucleosome
  let bare = H.HistoneState []
  putStrLn $ "Bare nucleosome: " ++ H.showHistoneState bare
  putStrLn ""

  -- Apply MLL1/2 writer -> H3K4me3 (chmod +x)
  let active = H.applyWriter H.MLL1_2 bare
  putStrLn "After MLL1/2 (writes H3K4me3 = chmod +x):"
  putStrLn $ "  " ++ H.showHistoneState active
  putStrLn ""

  -- Add P300/CBP -> H3K27ac (public access enhancer)
  let enhancer = H.applyWriter H.P300_CBP active
  putStrLn "After P300/CBP (writes H3K27ac = public access):"
  putStrLn $ "  " ++ H.showHistoneState enhancer
  putStrLn ""

  -- Now apply EZH2 (PRC2) -> H3K27me3 (chmod -r)
  -- Note: H3K27ac and H3K27me3 are mutually exclusive in biology
  -- We first need to remove H3K27ac
  let deacetylated = H.applyEraser H.HDAC1_2 enhancer
  putStrLn "After HDAC1/2 (erases H3K27ac):"
  putStrLn $ "  " ++ H.showHistoneState deacetylated
  putStrLn ""

  let repressed = H.applyWriter H.EZH2 deacetylated
  putStrLn "After EZH2/PRC2 (writes H3K27me3 = chmod -r):"
  putStrLn $ "  " ++ H.showHistoneState repressed
  putStrLn $ "  This is now BIVALENT: " ++ show (H.isBivalentState repressed)
  putStrLn ""

  -- Heterochromatin: SUV39H1 -> H3K9me3 (kernel lock)
  let hetero = H.applyWriter H.SUV39H1 (H.HistoneState [])
  putStrLn "After SUV39H1 (writes H3K9me3 = kernel lock):"
  putStrLn $ "  " ++ H.showHistoneState hetero
  putStrLn ""

  -- Reader domains
  putStrLn "Reader domain recognition:"
  putStrLn $ "  PHD finger reads active:   " ++ show (H.readMarks H.PHD active)
  putStrLn $ "  Chromo reads H3K9me3:      " ++ show (H.readMarks H.Chromo hetero)
  putStrLn $ "  Bromo reads acetylation:   " ++ show (H.readMarks H.Bromo enhancer)
  putStrLn ""

-- ============================================================
-- Demo 3: Bivalent Chromatin Resolution
-- ============================================================

demo3_bivalentResolution :: IO ()
demo3_bivalentResolution = do
  putStrLn "============================================"
  putStrLn "Demo 3: Bivalent Chromatin as Superposition"
  putStrLn "============================================"
  putStrLn ""

  -- Create bivalent state (H3K4me3 + H3K27me3)
  let bivalent = H.applyWriter H.EZH2 (H.applyWriter H.MLL1_2 (H.HistoneState []))

  putStrLn "Bivalent nucleosome (stem cell state):"
  putStrLn $ "  " ++ H.showHistoneState bivalent
  putStrLn $ "  Is bivalent: " ++ show (H.isBivalentState bivalent)
  putStrLn $ "  Like quantum superposition: |active> + |repressed>"
  putStrLn ""

  -- Resolution to active (neural differentiation)
  let resolvedActive = A.resolveBivalent True bivalent
  putStrLn "Resolution -> Active (e.g., neural differentiation of SOX2):"
  putStrLn $ "  " ++ H.showHistoneState resolvedActive
  putStrLn $ "  KDM6A removes H3K27me3, collapsing to |active>"
  putStrLn ""

  -- Resolution to repressed (e.g., muscle genes in neural cells)
  let resolvedRepressed = A.resolveBivalent False bivalent
  putStrLn "Resolution -> Repressed (e.g., muscle genes in neural cells):"
  putStrLn $ "  " ++ H.showHistoneState resolvedRepressed
  putStrLn $ "  KDM5A removes H3K4me3, collapsing to |repressed>"
  putStrLn ""

  -- Full state computation
  let methMap = M.MethylationMap [M.CpGSite 100 M.Unmethylated]
      histMap = H.HistoneMap [bivalent]
      acc     = A.computeAccessibility methMap histMap

  putStrLn "Full accessibility computation for bivalent region:"
  putStrLn $ "  Methylation: low"
  putStrLn $ "  Histones: bivalent"
  putStrLn $ "  Computed accessibility: " ++ showAccessibility acc
  putStrLn ""

-- ============================================================
-- Demo 4: Epigenetic Inheritance (State Serialization)
-- ============================================================

demo4_inheritance :: IO ()
demo4_inheritance = do
  putStrLn "============================================"
  putStrLn "Demo 4: Epigenetic Inheritance (Cell Division)"
  putStrLn "============================================"
  putStrLn ""

  -- Create a parent cell with specific epigenetic state
  let parentMeth = M.applyDNMT3 [100, 200, 300, 400, 500]
                   (M.MethylationMap [ M.CpGSite p M.Unmethylated | p <- [100, 200 .. 1000] ])
      parentHist = H.HistoneMap
                   [ H.applyWriter H.MLL1_2 (H.HistoneState [])
                   , H.applyWriter H.EZH2 (H.HistoneState [])
                   ]
      parentAcc  = A.computeAccessibility parentMeth parentHist
      parent     = EpigeneticState parentMeth parentHist parentAcc

  putStrLn "Parent cell state:"
  putStr   $ showState parent
  putStrLn ""

  -- Simulate cell division
  let result = A.simulateDivision parent
  putStrLn "After cell division:"
  putStrLn $ "  Faithfulness: " ++ show (A.faithfulness result) ++ " (95% DNMT1 fidelity)"
  putStrLn ""
  putStrLn "Daughter cell 1:"
  putStr   $ showState (A.daughterCell1 result)
  putStrLn ""
  putStrLn "Daughter cell 2:"
  putStr   $ showState (A.daughterCell2 result)
  putStrLn ""

  -- Serialization / deserialization (checkpoint)
  let serialized = A.serializeState parent
  putStrLn "State serialization (checkpoint):"
  putStrLn $ "  Serialized length: " ++ show (length serialized) ++ " chars"
  case A.deserializeState serialized of
    Just _  -> putStrLn "  Deserialization: SUCCESS (state restored)"
    Nothing -> putStrLn "  Deserialization: FAILED"
  putStrLn ""

-- ============================================================
-- Demo 5: X-Inactivation as Process Isolation
-- ============================================================

demo5_xInactivation :: IO ()
demo5_xInactivation = do
  putStrLn "============================================"
  putStrLn "Demo 5: X-Inactivation as Process Isolation"
  putStrLn "============================================"
  putStrLn ""

  -- Active X chromosome
  let activeX = EpigeneticState
        { methylation   = M.MethylationMap [ M.CpGSite p M.Unmethylated | p <- [100, 200 .. 500] ]
        , histones      = H.HistoneMap [ H.applyWriter H.MLL1_2 (H.HistoneState []) ]
        , accessibility = FullyOpen
        }

  putStrLn "Active X chromosome (Xa):"
  putStrLn $ "  " ++ A.showXState A.XActive
  putStr   $ showState activeX
  putStrLn ""

  -- X-inactivation process
  putStrLn "X-inactivation cascade:"
  putStrLn $ "  1. " ++ A.showXState A.XChoicePhase
  putStrLn $ "  2. " ++ A.showXState A.XInitiation
  putStrLn $ "  3. " ++ A.showXState A.XSpreading

  let (inactiveX, finalState) = A.xInactivate activeX
  putStrLn $ "  4. " ++ A.showXState finalState
  putStrLn ""

  putStrLn "Inactive X chromosome (Xi / Barr body):"
  putStr   $ showState inactiveX
  putStrLn ""

  -- Escapee genes
  putStrLn "~15% of X-linked genes escape inactivation:"
  putStrLn $ "  State: " ++ A.showXState A.XEscapee
  putStrLn "  (e.g., XIST itself, genes in pseudoautosomal regions)"
  putStrLn ""

  -- X-reactivation (in ICM)
  let (reactivatedX, rxState) = A.xReactivate inactiveX
  putStrLn "X-reactivation (inner cell mass of blastocyst):"
  putStrLn $ "  " ++ A.showXState rxState
  putStr   $ showState reactivatedX
  putStrLn ""

-- ============================================================
-- Demo 6: Chromatin Remodeling as Garbage Collection
-- ============================================================

demo6_remodeling :: IO ()
demo6_remodeling = do
  putStrLn "============================================"
  putStrLn "Demo 6: Chromatin Remodeling as GC/Defrag"
  putStrLn "============================================"
  putStrLn ""

  -- Cluttered chromatin state
  let cluttered = EpigeneticState
        { methylation   = M.applyDNMT3 [100, 200, 300]
                          (M.MethylationMap [ M.CpGSite p M.Unmethylated | p <- [100..500] ])
        , histones      = H.HistoneMap
                          [ H.applyWriter H.SUV39H1 (H.applyWriter H.MLL1_2 (H.HistoneState []))
                          , H.applyWriter H.EZH2 (H.HistoneState [])
                          , H.applyWriter H.P300_CBP (H.HistoneState [])
                          ]
        , accessibility = PartiallyRepressed
        }

  putStrLn "Cluttered chromatin (mixed signals):"
  putStr   $ showState cluttered
  putStrLn ""

  -- SWI/SNF remodeling (nucleosome eviction = GC)
  let swi_snf = A.Remodeler "BAF" A.SWI_SNF A.Evict
  putStrLn $ "Applying " ++ A.showRemodeler swi_snf ++ ":"
  let remodeled = A.applyRemodeler swi_snf cluttered
  putStr   $ showState remodeled
  putStrLn "  (Nucleosomes evicted, chromatin opened)"
  putStrLn ""

  -- Garbage collection (remove conflicting marks)
  let gcResult = A.garbageCollect cluttered
  putStrLn "After garbage collection (conflict resolution):"
  putStr   $ showState gcResult
  putStrLn ""

  -- Defragmentation (regularize spacing)
  let defragged = A.defragment gcResult
  putStrLn "After defragmentation (nucleosome spacing):"
  putStr   $ showState defragged
  putStrLn ""

-- ============================================================
-- Demo 7: Methylation Monad Composition
-- ============================================================

demo7_methylationMonad :: IO ()
demo7_methylationMonad = do
  putStrLn "============================================"
  putStrLn "Demo 7: Methylation as Monad"
  putStrLn "============================================"
  putStrLn ""

  -- Pure value (unmethylated = return/pure)
  let pureExpr = return "Gene ON" :: M.MethylationMonad String
  putStrLn "return \"Gene ON\" (unmethylated = default permissive):"
  let (ctx1, val1) = M.runMethylation pureExpr
  putStrLn $ "  Value: " ++ val1
  putStrLn $ "  Context: " ++ M.showMethylationMap ctx1
  putStrLn ""

  -- Bind: methylation-dependent gating
  let methylated = M.MethylationMonad
        (M.applyDNMT3 [100, 200] M.emptyMethylationMap)
        "Gene expression level"

  let gated = methylated >>= \expr ->
        let ctx = M.applyDNMT3 [300] M.emptyMethylationMap
        in M.MethylationMonad ctx (expr ++ " [gated by methylation]")

  putStrLn "Methylation-dependent binding (>>= gates expression):"
  let (ctx2, val2) = M.runMethylation gated
  putStrLn $ "  Value: " ++ val2
  putStrLn $ "  Context: " ++ M.showMethylationMap ctx2
  putStrLn ""

  -- Monadic composition: chain of methylation events
  let chain = do
        let s1 = M.MethylationMonad
                   (M.applyDNMT3 [100] M.emptyMethylationMap)
                   "Step 1: Promoter methylated"
        s2 <- s1
        M.MethylationMonad
          (M.applyDNMT3 [200, 300] M.emptyMethylationMap)
          (s2 ++ " -> Step 2: Spreading")

  putStrLn "Monadic chain (methylation spreading):"
  let (ctx3, val3) = M.runMethylation chain
  putStrLn $ "  Value: " ++ val3
  putStrLn $ "  Context: " ++ M.showMethylationMap ctx3
  putStrLn ""

  -- Functor: fmap over methylation context
  let mapped = fmap (++ " [MAPPED]") methylated
  putStrLn "fmap over methylation context:"
  let (ctx4, val4) = M.runMethylation mapped
  putStrLn $ "  Value: " ++ val4
  putStrLn $ "  Context: " ++ M.showMethylationMap ctx4
  putStrLn ""

-- ============================================================
-- Main
-- ============================================================

main :: IO ()
main = do
  putStrLn "========================================================"
  putStrLn "  DNA-Lang: Epigenetic Marks as Runtime State"
  putStrLn "  A Type-Theoretic Framework for Chromatin Accessibility"
  putStrLn "========================================================"
  putStrLn ""

  demo1_methylation
  demo2_histoneModifications
  demo3_bivalentResolution
  demo4_inheritance
  demo5_xInactivation
  demo6_remodeling
  demo7_methylationMonad

  putStrLn "========================================================"
  putStrLn "  All demonstrations complete."
  putStrLn "========================================================"
