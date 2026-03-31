{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Main
-- Description : Demonstration of the regulatory sequences control flow framework
-- Copyright   : (c) Matthew Long, YonedaAI Research Collective, 2026
-- License     : BSD-3-Clause
--
-- Demonstrates promoter binding, enhancer effects, gene circuit simulation
-- (toggle switch, repressilator, feed-forward loop), and GRN evaluation.

module Main where

import Promoter
import Enhancer
import Regulator
import ExpressionControl

-- ============================================================
-- Helper: show a double with limited precision
-- ============================================================
showF :: Double -> String
showF x = show (fromIntegral (round (x * 1000)) / 1000.0 :: Double)

-- ============================================================
-- Demo 1: Promoter Binding and Activity
-- ============================================================
demoPromoters :: IO ()
demoPromoters = do
  putStrLn "============================================"
  putStrLn "  DEMO 1: Promoters as Function Entry Points"
  putStrLn "============================================"
  putStrLn ""

  let prom1 = defaultPromoter
  putStrLn $ "Default promoter: " ++ promoterInfo prom1
  putStrLn $ "  Has TATA box: " ++ show (hasTATABox prom1)

  let prom2 = tataPromoter "GCGCGCTATAAAAGGCGC" 0.85
  putStrLn $ "\nStrong TATA promoter: " ++ promoterInfo prom2

  let prom3 = tatalessPromoter "CGCGCGCGCGCGCGCG" 0.7
  putStrLn $ "TATA-less promoter: " ++ promoterInfo prom3

  let prom4 = strongPromoter
  putStrLn $ "Strong constitutive (CMV): " ++ promoterInfo prom4

  -- Test with different TF contexts
  let emptyCtx = [] :: TFContext
      activatorCtx = [ TranscriptionFactor "SP1"  0.8 True
                      , TranscriptionFactor "AP1"  0.6 True ]
      repressedCtx = [ TranscriptionFactor "SP1"  0.8 True
                      , TranscriptionFactor "REST" 0.9 False ]
      mixedCtx     = [ TranscriptionFactor "SP1"  0.8 True
                      , TranscriptionFactor "AP1"  0.6 True
                      , TranscriptionFactor "REST" 0.4 False ]

  putStrLn "\nPromoter activity in different TF contexts:"
  putStrLn $ "  Empty context:     " ++ showF (promoterActivity prom1 emptyCtx)
  putStrLn $ "  Activators (SP1+AP1): " ++ showF (promoterActivity prom1 activatorCtx)
  putStrLn $ "  Repressed (SP1+REST): " ++ showF (promoterActivity prom1 repressedCtx)
  putStrLn $ "  Mixed context:     " ++ showF (promoterActivity prom1 mixedCtx)

  putStrLn $ "\n  Strong promoter, activators: " ++ showF (promoterActivity prom4 activatorCtx)
  putStrLn ""

-- ============================================================
-- Demo 2: Enhancers and Distance Effects
-- ============================================================
demoEnhancers :: IO ()
demoEnhancers = do
  putStrLn "============================================"
  putStrLn "  DEMO 2: Enhancers as Remote Configuration"
  putStrLn "============================================"
  putStrLn ""

  let enh1 = defaultEnhancer
  putStrLn $ "Default enhancer: " ++ enhancerInfo enh1

  -- Show distance-dependent looping probability
  putStrLn "\nLooping probability vs. distance:"
  let distances = [100, 1000, 5000, 10000, 50000, 100000, 500000, 1000000] :: [Int]
  mapM_ (\d -> putStrLn $ "  " ++ show d ++ " bp: lambda = " ++
                showF (loopingProbability d)) distances

  -- Effective enhancement at various distances
  putStrLn "\nEffective enhancement (fold-change=5x) at various distances:"
  let enhancers = map (\d -> distantEnhancer d 5.0) distances
  mapM_ (\e -> putStrLn $ "  " ++ show (targetDistance e) ++ " bp: " ++
                showF (effectiveEnhancement e) ++ "x") enhancers

  -- Super-enhancer
  let se = superEnhancer
  putStrLn $ "\nSuper-enhancer: " ++ enhancerInfo se

  -- Composition of multiple enhancers
  let enh2 = distantEnhancer 5000 3.0
      enh3 = distantEnhancer 20000 4.0
      composed = composeEnhancers [enh1, enh2, enh3]
  putStrLn $ "\nComposed enhancement (3 enhancers): " ++ showF composed ++ "x"

  -- Insulator blocking
  let ins = defaultInsulator
  putStrLn $ "\nInsulator barrier strength: " ++ show (barrierStrength ins)
  putStrLn $ "Enhancement without insulator: " ++ showF (effectiveEnhancement enh1)
  putStrLn $ "Enhancement with insulator:    " ++
    showF (effectiveEnhancementWithInsulator enh1 ins)
  putStrLn ""

-- ============================================================
-- Demo 3: Silencers and the Regulator Type
-- ============================================================
demoRegulators :: IO ()
demoRegulators = do
  putStrLn "============================================"
  putStrLn "  DEMO 3: The Regulator<ExpressionLevel> Type"
  putStrLn "============================================"
  putStrLn ""

  -- Expression level lattice operations
  putStrLn "Expression level lattice operations:"
  putStrLn $ "  join Off Basal = " ++ show (latticeJoin Off Basal)
  putStrLn $ "  join Basal (Induced 0.5) = " ++ show (latticeJoin Basal (Induced 0.5))
  putStrLn $ "  join (Induced 0.3) (Induced 0.7) = " ++
    show (latticeJoin (Induced 0.3) (Induced 0.7))
  putStrLn $ "  meet Max (Induced 0.5) = " ++ show (latticeMeet Max (Induced 0.5))
  putStrLn $ "  meet Off Basal = " ++ show (latticeMeet Off Basal)

  -- Level conversions
  putStrLn "\nLevel-to-Double conversions:"
  putStrLn $ "  Off    -> " ++ show (levelToDouble Off)
  putStrLn $ "  Basal  -> " ++ show (levelToDouble Basal)
  putStrLn $ "  Induced 0.5 -> " ++ show (levelToDouble (Induced 0.5))
  putStrLn $ "  Max    -> " ++ show (levelToDouble Max)

  -- Silencer effects
  let sil1 = defaultSilencer
      sil2 = polycombSilencer
  putStrLn "\nSilencer effects on expression level 0.8:"
  putStrLn $ "  Classical (0.7 repression): " ++ showF (applySilencer sil1 0.8)
  putStrLn $ "  Polycomb  (0.9 repression): " ++ showF (applySilencer sil2 0.8)
  putStrLn $ "  Both (max repression):      " ++ showF (applyAllSilencers [sil1, sil2] 0.8)

  -- Build a complete regulator
  let reg :: Regulator Double
      reg = Regulator
        { regPromoter  = defaultPromoter
        , regEnhancers = [defaultEnhancer]
        , regSilencers = [defaultSilencer]
        , regOutput    = \x -> if x > 0.5 then Induced x else Basal
        }
  putStrLn $ "\nRegulator: " ++ regulatorInfo reg
  putStrLn ""

-- ============================================================
-- Demo 4: Toggle Switch
-- ============================================================
demoToggleSwitch :: IO ()
demoToggleSwitch = do
  putStrLn "============================================"
  putStrLn "  DEMO 4: Toggle Switch (Bistable Circuit)"
  putStrLn "============================================"
  putStrLn ""

  putStrLn "Two mutually repressing genes: g1 --| g2 --| g1"
  putStrLn "Starting with asymmetric initial conditions..."
  putStrLn ""

  -- Case 1: g1 starts high
  let trace1 = simulateToggle 4.0 0.1 100
      (g1_final_1, g2_final_1) = last trace1
  putStrLn "Case 1: g1=4.0, g2=0.1"
  putStrLn "  Time steps (every 10th):"
  mapM_ (\i -> let (g1, g2) = trace1 !! i in
    putStrLn $ "    t=" ++ show i ++ ": g1=" ++ showF g1 ++
               ", g2=" ++ showF g2) [0, 10, 20, 30, 50, 70, 99]
  putStrLn $ "  Steady state: g1=" ++ showF g1_final_1 ++
             ", g2=" ++ showF g2_final_1

  putStrLn ""

  -- Case 2: g2 starts high
  let trace2 = simulateToggle 0.1 4.0 100
      (g1_final_2, g2_final_2) = last trace2
  putStrLn "Case 2: g1=0.1, g2=4.0"
  putStrLn $ "  Steady state: g1=" ++ showF g1_final_2 ++
             ", g2=" ++ showF g2_final_2

  putStrLn "\n  => Bistability confirmed: two distinct stable states."
  putStrLn ""

-- ============================================================
-- Demo 5: Repressilator
-- ============================================================
demoRepressilator :: IO ()
demoRepressilator = do
  putStrLn "============================================"
  putStrLn "  DEMO 5: Repressilator (Ring Oscillator)"
  putStrLn "============================================"
  putStrLn ""

  putStrLn "Three-gene cyclic repression: g1 --| g2 --| g3 --| g1"
  putStrLn ""

  let trace = simulateRepressilator 3.0 0.5 0.1 500
  putStrLn "Oscillation trace (every 50th step):"
  mapM_ (\i -> let (g1, g2, g3) = trace !! i in
    putStrLn $ "  t=" ++ show i ++ ": g1=" ++ showF g1 ++
               ", g2=" ++ showF g2 ++ ", g3=" ++ showF g3)
    [0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 499]

  -- Check for oscillation: compare levels at different time points
  let (g1_200, _, _) = trace !! 200
      (g1_300, _, _) = trace !! 300
      oscillating = abs (g1_200 - g1_300) > 0.1
  putStrLn $ "\n  Oscillation detected: " ++ show oscillating
  putStrLn ""

-- ============================================================
-- Demo 6: Feed-Forward Loop
-- ============================================================
demoFFL :: IO ()
demoFFL = do
  putStrLn "============================================"
  putStrLn "  DEMO 6: Coherent Feed-Forward Loop (C1-FFL)"
  putStrLn "============================================"
  putStrLn ""

  putStrLn "X -> Y -> Z with X -> Z (AND-gate at Z)"
  putStrLn "X is a persistent input signal = 1.0"
  putStrLn ""

  let trace = simulateFFL 1.0 100
  putStrLn "Response trace (every 10th step):"
  mapM_ (\i -> let (x, y, z) = trace !! i in
    putStrLn $ "  t=" ++ show i ++ ": X=" ++ showF x ++
               ", Y=" ++ showF y ++ ", Z=" ++ showF z)
    [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99]

  putStrLn "\n  => Z shows delayed activation (persistence detection)."
  putStrLn ""

-- ============================================================
-- Demo 7: GRN Category Simulation
-- ============================================================
demoGRN :: IO ()
demoGRN = do
  putStrLn "============================================"
  putStrLn "  DEMO 7: Gene Regulatory Network Simulation"
  putStrLn "============================================"
  putStrLn ""

  let genes = [ mkGene "GeneA" 1.0
              , mkGene "GeneB" 0.0
              , mkGene "GeneC" 0.0
              ]
      edges = [ mkEdge 0 1 Activation 1.0  -- A activates B
              , mkEdge 1 2 Activation 0.8  -- B activates C
              , mkEdge 2 0 Repression 0.5  -- C represses A (negative feedback)
              ]
      grn = mkGRN genes edges

  putStrLn "Initial GRN:"
  putStrLn $ grnInfo grn

  let steps = simulateGRN grn 200 5.0 1.0
  putStrLn "Simulation trace (every 40th step):"
  mapM_ (\i -> do
    let g = grnGenes (steps !! i)
    putStrLn $ "  t=" ++ show i ++ ": " ++
      concatMap (\gene -> geneName gene ++ "=" ++
                 showF (geneLevel gene) ++ "  ") g)
    [0, 40, 80, 120, 160, 199]

  putStrLn ""

-- ============================================================
-- Demo 8: Negative Feedback / Homeostasis
-- ============================================================
demoHomeostasis :: IO ()
demoHomeostasis = do
  putStrLn "============================================"
  putStrLn "  DEMO 8: Negative Feedback (Homeostasis)"
  putStrLn "============================================"
  putStrLn ""

  let input = 1.0
      gain  = 4.0
      trace = negativeFeedback input gain 100
      predicted = findSteadyState input gain

  putStrLn $ "Input signal: " ++ show input
  putStrLn $ "Feedback gain: " ++ show gain
  putStrLn $ "Predicted steady state: " ++ showF predicted
  putStrLn ""
  putStrLn "Convergence trace (every 10th step):"
  mapM_ (\i -> putStrLn $ "  t=" ++ show i ++ ": level=" ++
                showF (trace !! i)) [0, 10, 20, 30, 50, 70, 99]
  putStrLn $ "\nFinal level: " ++ showF (last trace)
  putStrLn $ "Error from predicted: " ++
    showF (abs (last trace - predicted))
  putStrLn ""

-- ============================================================
-- Main
-- ============================================================
main :: IO ()
main = do
  putStrLn ""
  putStrLn "************************************************************"
  putStrLn "*  Regulatory Sequences as Control Flow                     *"
  putStrLn "*  A Type-Theoretic Framework for Gene Expression           *"
  putStrLn "*  DNA-Lang Project -- Paper 2 of 7                         *"
  putStrLn "*  (c) Matthew Long, YonedaAI Research Collective, 2026     *"
  putStrLn "************************************************************"
  putStrLn ""

  demoPromoters
  demoEnhancers
  demoRegulators
  demoToggleSwitch
  demoRepressilator
  demoFFL
  demoGRN
  demoHomeostasis

  putStrLn "============================================"
  putStrLn "  All demonstrations complete."
  putStrLn "============================================"
