{-
  Main.hs — Entry Point for DNA-Lang

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Demonstrates the full DNA-Lang pipeline:
    1. Parse a sample program (PCSK9_KO)
    2. Type check it
    3. Compile to 4 target artifacts
    4. Execute in simulation mode
-}

module Main where

import Types
import Parser
import TypeChecker
import Compiler
import Runtime
import Examples
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn banner
    putStrLn ""

    -- ── Phase 1: Parsing ──────────────────────────────────────
    putStrLn "╔══════════════════════════════════════════════════════════╗"
    putStrLn "║  PHASE 1: PARSING                                      ║"
    putStrLn "╚══════════════════════════════════════════════════════════╝"
    putStrLn ""
    putStrLn "Source program:"
    putStrLn (pcsk9Source)
    putStrLn ""

    -- Parse from source text
    case parseProgram pcsk9Source of
        Left err -> putStrLn ("Parse error: " ++ peMessage err
                             ++ "\n  near: " ++ peRemaining err)
        Right parsed -> putStrLn ("Parsed successfully: program " ++ progName parsed)

    -- Use the AST-defined version for the rest of the pipeline
    let prog = pcsk9Program
    putStrLn ""

    -- ── Phase 2: Type Checking ────────────────────────────────
    putStrLn "╔══════════════════════════════════════════════════════════╗"
    putStrLn "║  PHASE 2: TYPE CHECKING                                ║"
    putStrLn "╚══════════════════════════════════════════════════════════╝"
    putStrLn ""

    case typeCheck prog of
        Left err -> putStrLn ("Type error: " ++ show err)
        Right env -> do
            putStrLn "Type check PASSED."
            putStrLn ""
            putStrLn "Type environment:"
            mapM_ (\(name, ty) -> putStrLn ("  " ++ name ++ " : " ++ showType ty))
                  (envToList env)
    putStrLn ""

    -- ── Phase 3: Compilation ──────────────────────────────────
    putStrLn "╔══════════════════════════════════════════════════════════╗"
    putStrLn "║  PHASE 3: COMPILATION (4 Artifacts)                    ║"
    putStrLn "╚══════════════════════════════════════════════════════════╝"
    putStrLn ""
    putStrLn (compileToString prog)

    -- ── Phase 4: Runtime Execution ────────────────────────────
    putStrLn "╔══════════════════════════════════════════════════════════╗"
    putStrLn "║  PHASE 4: RUNTIME EXECUTION (Simulation)               ║"
    putStrLn "╚══════════════════════════════════════════════════════════╝"
    putStrLn ""
    let result = execute prog
    putStrLn (formatResult result)

    -- ── Additional Examples ───────────────────────────────────
    putStrLn "╔══════════════════════════════════════════════════════════╗"
    putStrLn "║  ADDITIONAL EXAMPLES                                   ║"
    putStrLn "╚══════════════════════════════════════════════════════════╝"
    putStrLn ""

    putStrLn "--- Base Edit Program (Sickle Cell Disease) ---"
    runPipeline baseEditProgram
    putStrLn ""

    putStrLn "--- Gene Activation Program (CRISPRa) ---"
    runPipeline geneActivationProgram

    putStrLn ""
    putStrLn "DNA-Lang pipeline complete."

-- | Run the full pipeline for a program (type check + compile summary).
runPipeline :: Program -> IO ()
runPipeline prog = do
    putStrLn ("Program: " ++ progName prog)
    case typeCheck prog of
        Left err -> putStrLn ("  Type error: " ++ show err)
        Right _  -> do
            putStrLn "  Type check: PASSED"
            let artifacts = compile prog
            mapM_ (\a -> putStrLn ("  Artifact: " ++ showArtKind (artKind a)
                                  ++ " (" ++ show (length (artContent a)) ++ " entries)"))
                  artifacts
            let result = execute prog
            putStrLn ("  Execution: " ++ (if rrSuccess result then "SUCCESS" else "FAILED")
                     ++ " (" ++ show (rsStepCount (rrState result)) ++ " steps)")

showArtKind :: ArtifactKind -> String
showArtKind DesignArtifact     = "Design"
showArtKind AIArtifact         = "AI"
showArtKind ExperimentArtifact = "Experiment"
showArtKind ComplianceArtifact = "Compliance"

-- | Convert type environment to a list for display.
envToList :: TypeEnv -> [(String, DNAType)]
envToList = Map.toAscList

banner :: String
banner = Prelude.unlines
    [ "╔══════════════════════════════════════════════════════════╗"
    , "║                                                        ║"
    , "║   DNA-Lang v0.1.0                                      ║"
    , "║   A Typed Orchestration Language for Biological         ║"
    , "║   Programming                                          ║"
    , "║                                                        ║"
    , "║   (c) 2026 Matthew Long                                ║"
    , "║   YonedaAI Research Collective                         ║"
    , "║                                                        ║"
    , "╚══════════════════════════════════════════════════════════╝"
    ]
