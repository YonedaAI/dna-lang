{-
  Runtime.hs — Runtime Environment for DNA-Lang

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Runtime environment for executing DNA-Lang programs in simulation
  mode. Provides simulated biological operations, AI model invocations,
  and CRISPR editing workflows.
-}

module Runtime (
    execute,
    RuntimeState(..),
    RuntimeResult(..),
    SimulationLog,
    initialState,
    formatResult
) where

import Types
import qualified Data.Map as Map

-- ══════════════════════════════════════════════════════════════════
-- Runtime State
-- ══════════════════════════════════════════════════════════════════

data RuntimeState = RuntimeState
    { rsBindings  :: Map.Map String RuntimeValue
    , rsLog       :: [String]
    , rsStepCount :: Int
    }
    deriving (Show)

data RuntimeValue
    = RVString String
    | RVInt Int
    | RVFloat Double
    | RVBool Bool
    | RVList [RuntimeValue]
    | RVRecord [(String, RuntimeValue)]
    | RVSequence SeqKind String
    | RVGuide String Double Double     -- sequence, specificity, efficiency
    | RVEdit EditKind String           -- kind, description
    | RVStructure String Double        -- PDB id, confidence
    | RVRisk RiskKind Double           -- kind, score
    | RVPhenotype String               -- description
    | RVNull
    deriving (Show)

type SimulationLog = [String]

data RuntimeResult = RuntimeResult
    { rrState   :: RuntimeState
    , rrOutput  :: String
    , rrSuccess :: Bool
    }
    deriving (Show)

initialState :: RuntimeState
initialState = RuntimeState Map.empty [] 0

-- ══════════════════════════════════════════════════════════════════
-- Execution
-- ══════════════════════════════════════════════════════════════════

execute :: Program -> RuntimeResult
execute prog =
    let state0 = initialState
        state1 = logStep state0 ("=== Executing program: " ++ progName prog ++ " ===")
        state2 = foldl executeDecl state1 (progDecls prog)
        state3 = foldl executeAction state2 (progActions prog)
        state4 = logStep state3 ("=== Program " ++ progName prog ++ " completed ===")
        output = Prelude.unlines (reverse (rsLog state4))
    in RuntimeResult state4 output True

-- ══════════════════════════════════════════════════════════════════
-- Declaration Execution
-- ══════════════════════════════════════════════════════════════════

executeDecl :: RuntimeState -> Decl -> RuntimeState
executeDecl st (DTarget name expr) =
    let val = evalExpr st expr
        st' = bind name val st
    in logStep st' ("[DECL] target " ++ name ++ " = " ++ showRV val)

executeDecl st (DSequence name kind expr) =
    let val = case evalExpr st expr of
            RVString s -> RVSequence kind s
            v          -> v
        st' = bind name val st
    in logStep st' ("[DECL] sequence " ++ name ++ " : " ++ show kind ++ " = " ++ showRV val)

executeDecl st (DGuideRNA name expr) =
    let val = case evalExpr st expr of
            v@(RVGuide _ _ _) -> v
            _                 -> simulateGuideDesign name
        st' = bind name val st
    in logStep st' ("[DECL] guide " ++ name ++ " = " ++ showRV val)

executeDecl st (DEdit name kind expr) =
    let val = RVEdit kind (showRV (evalExpr st expr))
        st' = bind name val st
    in logStep st' ("[DECL] edit " ++ name ++ " : " ++ show kind ++ " = " ++ showRV val)

executeDecl st (DVector name expr) =
    let val = evalExpr st expr
        st' = bind name val st
    in logStep st' ("[DECL] vector " ++ name ++ " = " ++ showRV val)

executeDecl st (DCellLine name expr) =
    let val = evalExpr st expr
        st' = bind name val st
    in logStep st' ("[DECL] cell_line " ++ name ++ " = " ++ showRV val)

executeDecl st (DAssay name expr) =
    let val = evalExpr st expr
        st' = bind name val st
    in logStep st' ("[DECL] assay " ++ name ++ " = " ++ showRV val)

executeDecl st (DModelResult name expr) =
    let val = evalExpr st expr
        st' = bind name val st
    in logStep st' ("[DECL] model_result " ++ name ++ " = " ++ showRV val)

executeDecl st (DConstruct name expr) =
    let val = evalExpr st expr
        st' = bind name val st
    in logStep st' ("[DECL] construct " ++ name ++ " = " ++ showRV val)

executeDecl st (DCandidate name expr) =
    let val = evalExpr st expr
        st' = bind name val st
    in logStep st' ("[DECL] candidate " ++ name ++ " = " ++ showRV val)

executeDecl st (DLet name _ expr) =
    let val = evalExpr st expr
        st' = bind name val st
    in logStep st' ("[DECL] let " ++ name ++ " = " ++ showRV val)

executeDecl st (DRequire constraint) =
    logStep st ("[CHECK] require " ++ showConstraint constraint ++ " -> PASS (simulated)")

-- ══════════════════════════════════════════════════════════════════
-- Action Execution
-- ══════════════════════════════════════════════════════════════════

executeAction :: RuntimeState -> Action -> RuntimeState
executeAction st (ARun name args) =
    let argVals = map (evalExpr st) args
        result  = simulateFunction name argVals
        st'     = bind ("_result_" ++ name) result st
    in logStep st' ("[RUN] " ++ name ++ "(" ++ showRVList argVals ++ ") -> " ++ showRV result)

executeAction st (ACollect name expr) =
    let val = evalExpr st expr
        st' = bind name val st
    in logStep st' ("[COLLECT] " ++ name ++ " = " ++ showRV val)

executeAction st (AValidate expr) =
    let val = evalExpr st expr
    in logStep st ("[VALIDATE] " ++ showRV val ++ " -> PASS (simulated)")

executeAction st (ALog expr) =
    let val = evalExpr st expr
    in logStep st ("[LOG] " ++ showRV val)

executeAction st (AReturn expr) =
    let val = evalExpr st expr
    in logStep st ("[RETURN] " ++ showRV val)

-- ══════════════════════════════════════════════════════════════════
-- Expression Evaluation
-- ══════════════════════════════════════════════════════════════════

evalExpr :: RuntimeState -> Expr -> RuntimeValue
evalExpr _ (EStringLit s)  = RVString s
evalExpr _ (EIntLit n)     = RVInt n
evalExpr _ (EFloatLit d)   = RVFloat d
evalExpr _ (EBoolLit b)    = RVBool b
evalExpr st (EVar name)    =
    case Map.lookup name (rsBindings st) of
        Just v  -> v
        Nothing -> RVString name   -- Treat as string literal if unbound
evalExpr st (EList es) = RVList (map (evalExpr st) es)
evalExpr st (ERecord fs) = RVRecord (map (\(k,v) -> (k, evalExpr st v)) fs)
evalExpr st (EApp fname args) =
    let argVals = map (evalExpr st) args
    in simulateFunction fname argVals
evalExpr st (EField e field) =
    case evalExpr st e of
        RVRecord fs -> case lookup field fs of
            Just v  -> v
            Nothing -> RVNull
        _ -> RVNull
evalExpr st (EBinOp _ a b) =
    let va = evalExpr st a
        vb = evalExpr st b
    in RVBool True  -- Simulated comparison
evalExpr st (ETyped e _) = evalExpr st e

-- ══════════════════════════════════════════════════════════════════
-- Simulated Biological Functions
-- ══════════════════════════════════════════════════════════════════

simulateFunction :: String -> [RuntimeValue] -> RuntimeValue
simulateFunction "predict_structure" _ =
    RVStructure "AF-SIM-001" 0.89

simulateFunction "rank_guides" _ =
    RVList [ RVGuide "ACGTACGTACGTACGTACGT" 0.95 0.88
           , RVGuide "TGCATGCATGCATGCATGCA" 0.91 0.85
           , RVGuide "GCTAGCTAGCTAGCTAGCTA" 0.88 0.92
           ]

simulateFunction "predict_off_target" _ =
    RVRisk OffTarget 0.02

simulateFunction "propose_variants" _ =
    RVList [ RVSequence Protein "VARIANT_1_OPTIMIZED"
           , RVSequence Protein "VARIANT_2_STABILIZED"
           ]

simulateFunction "design_guide" _ =
    RVGuide "GCAGTCAGCTGACGATCGAT" 0.93 0.87

simulateFunction "knockout" _ =
    RVEdit Knockout "NHEJ-mediated disruption at target locus"

simulateFunction "base_edit" _ =
    RVEdit BaseEdit "C>T conversion at position 127"

simulateFunction "prime_edit" _ =
    RVEdit PrimeEdit "Precise 3bp insertion at target site"

simulateFunction "insert_payload" _ =
    RVEdit Insertion "Transgene inserted at safe harbor locus"

simulateFunction "assemble_construct" _ =
    RVRecord [ ("backbone", RVString "pAAV-CMV")
             , ("payload", RVString "Cas9 + guide expression cassette")
             , ("size_bp", RVInt 4200)
             ]

simulateFunction "run_assay" _ =
    RVPhenotype "72h post-transfection: 78% editing efficiency by NGS"

simulateFunction "validate_safety" _ =
    RVList [ RVRisk OffTarget 0.003
           , RVRisk Toxicity 0.01
           , RVRisk Immunogenicity 0.05
           ]

simulateFunction "generate_report" _ =
    RVRecord [ ("status", RVString "PASS")
             , ("editing_efficiency", RVFloat 0.78)
             , ("off_target_rate", RVFloat 0.003)
             , ("recommendation", RVString "Proceed to in vivo studies")
             ]

simulateFunction "lookup_sequence" _ =
    RVSequence DNA "ATGGTGCATCTGACTCCTGAGGAGAAGTCTGCCGTTACTGCCCTGTGGGGCAAGGTG"

simulateFunction "select_cell_line" _ =
    RVRecord [ ("name", RVString "HepG2")
             , ("origin", RVString "Human hepatocellular carcinoma")
             , ("ATCC", RVString "HB-8065")
             ]

simulateFunction "design_assay" _ =
    RVRecord [ ("type", RVString "NGS amplicon sequencing")
             , ("target_amplicon", RVString "500bp around cut site")
             , ("min_reads", RVInt 10000)
             ]

simulateFunction name _ =
    RVString ("[simulated: " ++ name ++ "]")

simulateGuideDesign :: String -> RuntimeValue
simulateGuideDesign _ = RVGuide "ACGTACGTACGTACGTACGT" 0.93 0.87

-- ══════════════════════════════════════════════════════════════════
-- Helpers
-- ══════════════════════════════════════════════════════════════════

bind :: String -> RuntimeValue -> RuntimeState -> RuntimeState
bind name val st = st { rsBindings = Map.insert name val (rsBindings st) }

logStep :: RuntimeState -> String -> RuntimeState
logStep st msg = st
    { rsLog = ("[step " ++ show (rsStepCount st + 1) ++ "] " ++ msg) : rsLog st
    , rsStepCount = rsStepCount st + 1
    }

showRV :: RuntimeValue -> String
showRV (RVString s)      = s
showRV (RVInt n)         = show n
showRV (RVFloat d)       = show d
showRV (RVBool b)        = show b
showRV (RVList vs)       = "[" ++ intercalate' ", " (map showRV vs) ++ "]"
showRV (RVRecord fs)     = "{ " ++ intercalate' ", " (map (\(k,v) -> k ++ "=" ++ showRV v) fs) ++ " }"
showRV (RVSequence k s)  = show k ++ ":" ++ s
showRV (RVGuide seq' sp eff) = "Guide(" ++ seq' ++ ", spec=" ++ show sp ++ ", eff=" ++ show eff ++ ")"
showRV (RVEdit k desc)   = show k ++ ":" ++ desc
showRV (RVStructure pid conf) = "Structure(" ++ pid ++ ", conf=" ++ show conf ++ ")"
showRV (RVRisk k score)  = "Risk<" ++ show k ++ ">(" ++ show score ++ ")"
showRV (RVPhenotype desc) = "Phenotype(" ++ desc ++ ")"
showRV RVNull            = "null"

showRVList :: [RuntimeValue] -> String
showRVList vs = intercalate' ", " (map showRV vs)

intercalate' :: String -> [String] -> String
intercalate' _ []     = ""
intercalate' _ [x]    = x
intercalate' sep (x:xs) = x ++ sep ++ intercalate' sep xs

formatResult :: RuntimeResult -> String
formatResult rr =
    let border = replicate 60 '-'
    in Prelude.unlines
        [ border
        , "  RUNTIME EXECUTION RESULT"
        , border
        , "  Success: " ++ show (rrSuccess rr)
        , "  Steps: " ++ show (rsStepCount (rrState rr))
        , border
        , ""
        , rrOutput rr
        ]
