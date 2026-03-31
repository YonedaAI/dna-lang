{-
  Main.hs -- Demonstrates Hox gene collinearity, morphogen gradient
             interpretation, cell fate decision tree, Waddington
             landscape simulation

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Developmental Programs as Orchestration
-}

module Main where

import BodyPlan
import RegulatoryNetwork
import Differentiation
import Program

main :: IO ()
main = do
  putStrLn "============================================================"
  putStrLn "  DNA-Lang Paper 7: Developmental Programs as Orchestration"
  putStrLn "============================================================"
  putStrLn ""

  -- 1. Hox Gene Collinearity
  demonstrateHoxCollinearity

  -- 2. Morphogen Gradient Interpretation
  demonstrateMorphogenGradient

  -- 3. Regulatory Boot Sequence
  demonstrateBootSequence

  -- 4. Cell Fate Decision Tree
  demonstrateCellFate

  -- 5. Waddington Landscape
  demonstrateWaddington

  -- 6. Apoptosis
  demonstrateApoptosis

  -- 7. Stem Cell Factory
  demonstrateStemCellFactory

  -- 8. Full Developmental Program
  demonstrateFullProgram

  putStrLn "============================================================"
  putStrLn "  All demonstrations complete."
  putStrLn "============================================================"

-- | Demonstrate Hox gene collinearity and the monotone functor property
demonstrateHoxCollinearity :: IO ()
demonstrateHoxCollinearity = do
  putStrLn "--- 1. Hox Gene Collinearity (Monotone Functor) ---"
  putStrLn ""
  putStrLn (showHoxCluster mammalianHoxA)
  putStrLn ""

  let functor = collinearityFunctor mammalianHoxA
  putStrLn "Collinearity Functor F: Chr -> Axis"
  mapM_ (\(chrPos, axPos) ->
    putStrLn ("  F(" ++ show chrPos ++ ") = " ++ show (axisValue axPos)))
    functor
  putStrLn ""

  putStrLn ("Spatial collinearity verified: "
    ++ show (verifySpatialCollinearity mammalianHoxA))
  putStrLn ("Temporal collinearity verified: "
    ++ show (verifyTemporalCollinearity mammalianHoxA))
  putStrLn ("Both collinearities (monotone functor): "
    ++ show (verifyCollinearity mammalianHoxA))
  putStrLn ""

-- | Demonstrate morphogen gradient interpretation (French flag model)
demonstrateMorphogenGradient :: IO ()
demonstrateMorphogenGradient = do
  putStrLn "--- 2. Morphogen Gradient (Distributed Configuration) ---"
  putStrLn ""

  let shh = MorphogenField
        { morphogenName  = "Sonic Hedgehog (Shh)"
        , sourcePosition = 0.0
        , decayRate      = 3.0
        , highThreshold  = 0.7
        , midThreshold   = 0.3
        , lowThreshold   = 0.1
        }

  putStrLn (showMorphogenField shh)

  let bmp = MorphogenField
        { morphogenName  = "BMP4"
        , sourcePosition = 1.0
        , decayRate      = 2.5
        , highThreshold  = 0.6
        , midThreshold   = 0.25
        , lowThreshold   = 0.08
        }

  putStrLn (showMorphogenField bmp)
  putStrLn ""

-- | Demonstrate regulatory network boot sequences
demonstrateBootSequence :: IO ()
demonstrateBootSequence = do
  putStrLn "--- 3. Regulatory Cascades (Boot Sequences) ---"
  putStrLn ""

  putStrLn (showNetwork myogenicCascade)
  putStrLn (showBootSequence myogenicCascade)
  putStrLn ""

  putStrLn (showNetwork neuralCascade)
  putStrLn (showBootSequence neuralCascade)
  putStrLn ""

-- | Demonstrate cell fate decisions (type refinement)
demonstrateCellFate :: IO ()
demonstrateCellFate = do
  putStrLn "--- 4. Cell Fate Decision Tree (Type Refinement) ---"
  putStrLn ""

  let zygote = Cell "zygote" (Stem Totipotent) [] 0
  putStrLn ("Start:        " ++ showCell zygote)

  let specified = specifyCell zygote "Neural"
  putStrLn ("Specified:    " ++ showCell specified)

  let determined = determineCell specified "Cortical_Neuron"
  putStrLn ("Determined:   " ++ showCell determined)

  let terminal = differentiateCell determined
  putStrLn ("Differentiated: " ++ showCell terminal)

  putStrLn ""
  putStrLn "Type narrowing chain:"
  putStrLn "  Stem(Totipotent) >: Progenitor(Neural) >: Determined(Cortical_Neuron) >: Terminal(Cortical_Neuron)"
  putStrLn ""

-- | Demonstrate the Waddington landscape
demonstrateWaddington :: IO ()
demonstrateWaddington = do
  putStrLn "--- 5. Waddington Landscape (Refinement Type Lattice) ---"
  putStrLn ""
  putStrLn (showLandscape hematopoieticLandscape)
  putStrLn ""

  -- Simulate a cell choosing the myeloid -> erythrocyte path
  let path1 = simulateWaddington hematopoieticLandscape [0, 0]
  putStrLn "Path 1 (Myeloid -> Erythrocyte):"
  putStrLn (showDifferentiationPath path1)

  -- Simulate a cell choosing the lymphoid -> T-cell path
  let path2 = simulateWaddington hematopoieticLandscape [1, 0]
  putStrLn "Path 2 (Lymphoid -> T-Cell):"
  putStrLn (showDifferentiationPath path2)

-- | Demonstrate apoptosis as graceful shutdown
demonstrateApoptosis :: IO ()
demonstrateApoptosis = do
  putStrLn "--- 6. Apoptosis (Graceful Shutdown) ---"
  putStrLn ""

  let cell = Cell "interdigital_01" (Determined "WebCell") ["BMP_signal"] 5
  putStrLn ("Before: " ++ showCell cell)

  let dying = triggerApoptosis cell "sculpting"
  putStrLn ("After signal: " ++ showCell dying)

  putStrLn ""
  putStrLn "Shutdown protocol (SIGTERM -> cleanup):"
  let stages = executeApoptosis cell
  mapM_ (\(stage, msg) ->
    putStrLn ("  [" ++ show stage ++ "] " ++ msg)) stages
  putStrLn ""

-- | Demonstrate stem cells as factory patterns
demonstrateStemCellFactory :: IO ()
demonstrateStemCellFactory = do
  putStrLn "--- 7. Stem Cells (Factory Pattern) ---"
  putStrLn ""

  let hsc = Cell "HSC_001" (Stem Multipotent) [] 0
  putStrLn ("Stem cell: " ++ showCell hsc)

  -- Symmetric division (self-renewal)
  let (copy1, copy2) = stemCellDivide hsc
  putStrLn ""
  putStrLn "Symmetric division (self-renewal):"
  putStrLn ("  Daughter 1: " ++ showCell copy1)
  putStrLn ("  Daughter 2: " ++ showCell copy2)

  -- Asymmetric division (stem + progenitor)
  let (stemDaughter, progenitorDaughter) = asymmetricDivide hsc "Myeloid"
  putStrLn ""
  putStrLn "Asymmetric division (factory pattern):"
  putStrLn ("  Stem daughter:      " ++ showCell stemDaughter)
  putStrLn ("  Progenitor daughter: " ++ showCell progenitorDaughter)

  -- Further differentiate the progenitor
  let determined = determineCell progenitorDaughter "Erythrocyte"
  let terminal = differentiateCell determined
  putStrLn ""
  putStrLn "Continuing differentiation of progenitor:"
  putStrLn ("  Determined: " ++ showCell determined)
  putStrLn ("  Terminal:   " ++ showCell terminal)
  putStrLn ""

-- | Demonstrate the full developmental program
demonstrateFullProgram :: IO ()
demonstrateFullProgram = do
  putStrLn "--- 8. Full Developmental Program ---"
  putStrLn ""

  let shh = MorphogenField "Shh" 0.0 3.0 0.7 0.3 0.1
  let bmp = MorphogenField "BMP4" 1.0 2.5 0.6 0.25 0.08

  let prog = initProgram mammalianHoxA [shh, bmp] myogenicCascade

  putStrLn (showProgram prog)
  putStrLn ""

  -- Run development
  let stages = runDevelopment prog
  putStrLn ("Developmental stages executed: " ++ show (length stages))
  mapM_ (\p -> do
    putStrLn ("  Stage: " ++ show (developmental_stage p)
           ++ " | Cells: " ++ show (length (cellPopulation p))
           ++ " | Ops: " ++ show (stageOps (developmental_stage p))))
    stages
  putStrLn ""

  -- Stage safety check
  putStrLn "Stage safety checks:"
  putStrLn ("  'divide' valid at Cleavage? "
    ++ show (isValidOp Cleavage "divide"))
  putStrLn ("  'divide' valid at Organogenesis? "
    ++ show (isValidOp Organogenesis "divide"))
  putStrLn ("  'differentiate' valid at Organogenesis? "
    ++ show (isValidOp Organogenesis "differentiate"))
  putStrLn ("  'apoptose' valid at Cleavage? "
    ++ show (isValidOp Cleavage "apoptose"))
  putStrLn ""
