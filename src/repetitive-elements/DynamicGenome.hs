{-
  DynamicGenome.hs -- Genome-level transposition simulation, composition tracking,
                      and domestication events

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Repetitive Elements as Self-Modifying Code
-}

module DynamicGenome
  ( -- Dynamic genome type
    DynamicGenome(..)
  , mkDynamicGenome
  , dgLength
  -- Element tracking
  , RepeatLocus(..)
  , addElement
  , removeElement
  , getElements
  , elementCount
  -- Composition statistics
  , GenomeComposition(..)
  , computeComposition
  , showComposition
  -- Transposition simulation
  , TranspositionEvent(..)
  , simulateCutPaste
  , simulateCopyPaste
  , simulateTPRT
  , applyEvents
  -- Domestication
  , DomesticationEvent(..)
  , domesticateElement
  , listDomesticated
  -- History
  , EventLog
  , showEventLog
  -- Example genomes
  , mkExampleGenome
  ) where

import Repeat
import Transposon
import SatelliteDNA

-- | A locus tracking a repeat element within the genome.
data RepeatLocus = RepeatLocus
  { rlPosition :: Int       -- start position in genome
  , rlElement  :: Repeat    -- the repeat element
  , rlName     :: String    -- identifier
  } deriving (Show, Eq)

-- | A transposition event record.
data TranspositionEvent
  = CutPasteEvent
    { cpeSource :: Int
    , cpeTarget :: Int
    , cpeLength :: Int
    , cpeName   :: String
    }
  | CopyPasteEvent
    { cpSource :: Int
    , cpTarget :: Int
    , cpLength :: Int
    , cpName   :: String
    }
  deriving (Show, Eq)

-- | A domestication event record.
data DomesticationEvent = DomesticationEvent
  { deName       :: String    -- element name
  , deFunction   :: String    -- acquired host function
  , dePosition   :: Int       -- locus
  } deriving (Show, Eq)

-- | Event log is a list of transposition or domestication descriptions.
type EventLog = [String]

-- | A dynamic genome with tracked repeat elements and event history.
data DynamicGenome = DynamicGenome
  { dgGenome   :: Genome
  , dgRepeats  :: [RepeatLocus]
  , dgHistory  :: EventLog
  } deriving (Show)

-- | Create a dynamic genome from a name, sequence string, and initial elements.
mkDynamicGenome :: String -> String -> DynamicGenome
mkDynamicGenome name seqStr = DynamicGenome
  { dgGenome  = mkGenome name seqStr
  , dgRepeats = []
  , dgHistory = ["Genome '" ++ name ++ "' initialized (" ++ show (length (mkSequence seqStr)) ++ " bp)"]
  }

-- | Current genome length.
dgLength :: DynamicGenome -> Int
dgLength = genomeLength . dgGenome

-- ─── Element Tracking ───────────────────────────────────────────────

-- | Add a repeat element at a given position.
addElement :: String -> Int -> Repeat -> DynamicGenome -> DynamicGenome
addElement name pos element dg =
  let locus = RepeatLocus pos element name
      msg = "Added " ++ repeatSummary element ++ " '" ++ name ++ "' at position " ++ show pos
  in dg { dgRepeats = locus : dgRepeats dg
        , dgHistory = dgHistory dg ++ [msg]
        }

-- | Remove an element by name.
removeElement :: String -> DynamicGenome -> DynamicGenome
removeElement name dg =
  let msg = "Removed element '" ++ name ++ "'"
  in dg { dgRepeats = filter (\rl -> rlName rl /= name) (dgRepeats dg)
        , dgHistory = dgHistory dg ++ [msg]
        }

-- | Get all tracked elements.
getElements :: DynamicGenome -> [RepeatLocus]
getElements = dgRepeats

-- | Count elements by type.
elementCount :: DynamicGenome -> (Int, Int, Int, Int)
elementCount dg =
  let els = map rlElement (dgRepeats dg)
      dnaT  = length [() | DNATransposon{}  <- els]
      retro = length [() | Retrotransposon{} <- els]
      sat   = length [() | SatelliteDNA{}    <- els]
      erv   = length [() | ERV{}             <- els]
  in (dnaT, retro, sat, erv)

-- ─── Genome Composition ─────────────────────────────────────────────

-- | Summary of genome composition.
data GenomeComposition = GenomeComposition
  { gcTotalLength     :: Int
  , gcDNATransposons  :: Int     -- total bp
  , gcRetrotransposons :: Int
  , gcSatellites      :: Int
  , gcERVs            :: Int
  , gcUniqueSequence  :: Int
  , gcRepeatFraction  :: Double
  } deriving (Show)

-- | Compute genome composition from tracked elements.
computeComposition :: DynamicGenome -> GenomeComposition
computeComposition dg =
  let total = dgLength dg
      elements = map rlElement (dgRepeats dg)
      dtBp    = sum [repeatLength e | e@DNATransposon{}  <- elements]
      rtBp    = sum [repeatLength e | e@Retrotransposon{} <- elements]
      satBp   = sum [repeatLength e | e@SatelliteDNA{}    <- elements]
      ervBp   = sum [repeatLength e | e@ERV{}             <- elements]
      repeatBp = dtBp + rtBp + satBp + ervBp
      uniqueBp = max 0 (total - repeatBp)
      fraction = if total > 0
                 then fromIntegral repeatBp / fromIntegral total
                 else 0.0
  in GenomeComposition total dtBp rtBp satBp ervBp uniqueBp fraction

-- | Pretty-print genome composition.
showComposition :: GenomeComposition -> String
showComposition gc = unlines
  [ "=== Genome Composition ==="
  , "Total length:       " ++ show (gcTotalLength gc) ++ " bp"
  , "DNA Transposons:    " ++ show (gcDNATransposons gc) ++ " bp ("
    ++ showPct (gcDNATransposons gc) (gcTotalLength gc) ++ ")"
  , "Retrotransposons:   " ++ show (gcRetrotransposons gc) ++ " bp ("
    ++ showPct (gcRetrotransposons gc) (gcTotalLength gc) ++ ")"
  , "Satellite DNA:      " ++ show (gcSatellites gc) ++ " bp ("
    ++ showPct (gcSatellites gc) (gcTotalLength gc) ++ ")"
  , "ERVs:               " ++ show (gcERVs gc) ++ " bp ("
    ++ showPct (gcERVs gc) (gcTotalLength gc) ++ ")"
  , "Unique sequence:    " ++ show (gcUniqueSequence gc) ++ " bp ("
    ++ showPct (gcUniqueSequence gc) (gcTotalLength gc) ++ ")"
  , "Repeat fraction:    " ++ showPctD (gcRepeatFraction gc)
  ]
  where
    showPct n d = if d == 0 then "0.0%" else
      let pct = (fromIntegral n / fromIntegral d * 100.0) :: Double
      in show (round pct :: Int) ++ "%"
    showPctD x = show (round (x * 100.0) :: Int) ++ "%"

-- ─── Transposition Simulation ───────────────────────────────────────

-- | Simulate a cut-and-paste transposition event.
simulateCutPaste :: String -> Int -> DynamicGenome -> DynamicGenome
simulateCutPaste elemName newPos dg =
  case findByName elemName (dgRepeats dg) of
    Nothing -> dg { dgHistory = dgHistory dg ++
      ["FAILED: Cut-and-paste of '" ++ elemName ++ "' - element not found"] }
    Just rl ->
      let oldPos = rlPosition rl
          len = repeatLength (rlElement rl)
          msg = "Cut-and-paste: '" ++ elemName ++ "' moved from "
                ++ show oldPos ++ " to " ++ show newPos
          updatedRepeats = map (\r -> if rlName r == elemName
                                      then r { rlPosition = newPos }
                                      else r) (dgRepeats dg)
          -- Apply to underlying genome
          mGenome = cutAndPaste oldPos len newPos (dgGenome dg)
      in case mGenome of
           Nothing -> dg { dgHistory = dgHistory dg ++
             ["FAILED: Cut-and-paste of '" ++ elemName ++ "' - invalid coordinates"] }
           Just g' -> dg { dgGenome = g'
                         , dgRepeats = updatedRepeats
                         , dgHistory = dgHistory dg ++ [msg]
                         }

-- | Simulate a copy-and-paste retrotransposition event.
simulateCopyPaste :: String -> Int -> String -> DynamicGenome -> DynamicGenome
simulateCopyPaste elemName targetPos newName dg =
  case findByName elemName (dgRepeats dg) of
    Nothing -> dg { dgHistory = dgHistory dg ++
      ["FAILED: Copy-and-paste of '" ++ elemName ++ "' - element not found"] }
    Just rl ->
      let srcPos = rlPosition rl
          len = repeatLength (rlElement rl)
          msg = "Copy-and-paste: '" ++ elemName ++ "' copied to position "
                ++ show targetPos ++ " as '" ++ newName ++ "'"
          newLocus = RepeatLocus targetPos (rlElement rl) newName
          mGenome = copyAndPaste srcPos len targetPos (dgGenome dg)
      in case mGenome of
           Nothing -> dg { dgHistory = dgHistory dg ++
             ["FAILED: Copy-and-paste of '" ++ elemName ++ "' - invalid coordinates"] }
           Just g' -> dg { dgGenome = g'
                         , dgRepeats = newLocus : dgRepeats dg
                         , dgHistory = dgHistory dg ++ [msg]
                         }

-- | Simulate target-primed reverse transcription (L1-style).
simulateTPRT :: String -> Int -> String -> DynamicGenome -> DynamicGenome
simulateTPRT elemName targetPos newName dg =
  case findByName elemName (dgRepeats dg) of
    Nothing -> dg { dgHistory = dgHistory dg ++
      ["FAILED: TPRT of '" ++ elemName ++ "' - element not found"] }
    Just rl ->
      if not (isAutonomous (rlElement rl))
      then dg { dgHistory = dgHistory dg ++
        ["FAILED: TPRT of '" ++ elemName ++ "' - element is not autonomous"] }
      else
        let srcPos = rlPosition rl
            len = repeatLength (rlElement rl)
            msg = "TPRT: '" ++ elemName ++ "' retrotransposed to position "
                  ++ show targetPos ++ " as '" ++ newName ++ "' (with TSD)"
            newLocus = RepeatLocus targetPos (rlElement rl) newName
            mGenome = targetPrimedRT srcPos len targetPos (dgGenome dg)
        in case mGenome of
             Nothing -> dg { dgHistory = dgHistory dg ++
               ["FAILED: TPRT of '" ++ elemName ++ "' - invalid coordinates"] }
             Just g' -> dg { dgGenome = g'
                           , dgRepeats = newLocus : dgRepeats dg
                           , dgHistory = dgHistory dg ++ [msg]
                           }

-- | Apply a list of transposition events.
applyEvents :: [TranspositionEvent] -> DynamicGenome -> DynamicGenome
applyEvents [] dg = dg
applyEvents (CutPasteEvent _ target _ name : rest) dg =
  applyEvents rest (simulateCutPaste name target dg)
applyEvents (CopyPasteEvent _ target _ name : rest) dg =
  applyEvents rest (simulateCopyPaste name target (name ++ "_copy") dg)

-- ─── Domestication ──────────────────────────────────────────────────

-- | Domesticate a tracked element, giving it a host function.
domesticateElement :: String -> String -> DynamicGenome -> DynamicGenome
domesticateElement elemName func dg =
  let msg = "Domestication: '" ++ elemName ++ "' acquired function '" ++ func ++ "'"
      updatedRepeats = map (\rl -> if rlName rl == elemName
                                   then rl { rlElement = domesticate (rlElement rl) func }
                                   else rl) (dgRepeats dg)
  in dg { dgRepeats = updatedRepeats
        , dgHistory = dgHistory dg ++ [msg]
        }

-- | List all domesticated elements.
listDomesticated :: DynamicGenome -> [(String, String)]
listDomesticated dg =
  [ (rlName rl, f)
  | rl <- dgRepeats dg
  , Domesticated f <- [getActivity (rlElement rl)]
  ]
  where
    getActivity (DNATransposon _ _ _ _ a)  = a
    getActivity (Retrotransposon _ _ _ _ a) = a
    getActivity (ERV _ _ _ _ _ _ a)         = a
    getActivity _                           = Fossilized  -- satellites have no activity

-- ─── History ────────────────────────────────────────────────────────

-- | Pretty-print the event log.
showEventLog :: DynamicGenome -> String
showEventLog dg = unlines $
  ["=== Event Log ==="] ++
  zipWith (\i msg -> show i ++ ". " ++ msg) [(1::Int)..] (dgHistory dg)

-- ─── Helper ─────────────────────────────────────────────────────────

findByName :: String -> [RepeatLocus] -> Maybe RepeatLocus
findByName _ [] = Nothing
findByName name (rl:rls)
  | rlName rl == name = Just rl
  | otherwise = findByName name rls

-- ─── Example Genome ─────────────────────────────────────────────────

-- | Create an example genome with various repeat elements for demonstration.
mkExampleGenome :: DynamicGenome
mkExampleGenome =
  let -- A simplified genome sequence (1000 bp of varied sequence)
      baseSeq = take 1000 (cycle "ATCGATCGATCGAATTCCGGAATCGATCG")
      dg0 = mkDynamicGenome "ExampleGenome" baseSeq

      -- Add a DNA transposon (Tc1/mariner-like)
      tc1 = DNATransposon
        { dtTirLeft     = mkSequence "ACAGTG"
        , dtTirRight    = mkSequence "CACTGT"  -- reverse complement
        , dtBody        = mkSequence (replicate 80 'A')  -- simplified
        , dtTransposase = Gene "Tc1_transposase" (mkSequence (replicate 60 'A'))
        , dtActivity    = Active
        }

      -- Add an autonomous LINE (L1-like)
      l1 = Retrotransposon
        { rtClass_              = LINE
        , rtBody                = mkSequence (take 300 (cycle "ATCGATCG"))
        , rtReverseTranscriptase = Just (Gene "L1_RT" (mkSequence (replicate 100 'A')))
        , rtPolyA               = True
        , rtActivity            = Active
        }

      -- Add a non-autonomous SINE (Alu-like)
      alu = Retrotransposon
        { rtClass_              = SINE
        , rtBody                = mkSequence (take 150 (cycle "GCGCGCATATATAT"))
        , rtReverseTranscriptase = Nothing
        , rtPolyA               = True
        , rtActivity            = Active
        }

      -- Add centromeric satellite
      centromere = mkAlphaSatellite 5  -- 5 copies for demo

      -- Add telomeric repeats
      telomere = mkTelomericRepeat 100  -- 100 TTAGGG repeats

      -- Add an ERV
      erv1 = ERV
        { ervLtrLeft   = mkSequence (take 50 (cycle "ATCGATCG"))
        , ervLtrRight  = mkSequence (take 50 (cycle "ATCGATCG"))
        , ervGag       = Just (Gene "gag" (mkSequence (replicate 40 'A')))
        , ervPol       = Just (Gene "pol" (mkSequence (replicate 60 'A')))
        , ervEnv       = Just (Gene "env" (mkSequence (replicate 40 'A')))
        , ervIntact    = False
        , ervActivity  = Fossilized
        }

      -- A domesticated transposon (RAG1-like)
      rag1 = DNATransposon
        { dtTirLeft     = mkSequence "CACAGTG"
        , dtTirRight    = mkSequence "CACTGTG"
        , dtBody        = mkSequence (take 200 (cycle "ATCGATCG"))
        , dtTransposase = Gene "RAG1" (mkSequence (replicate 100 'A'))
        , dtActivity    = Domesticated "V(D)J recombination"
        }

      dg1 = addElement "Tc1"        100 tc1        dg0
      dg2 = addElement "L1_001"     200 l1         dg1
      dg3 = addElement "Alu_001"    350 alu        dg2
      dg4 = addElement "CEN_alpha"  500 centromere  dg3
      dg5 = addElement "TEL_q"      900 telomere   dg4
      dg6 = addElement "HERV_K1"    600 erv1       dg5
      dg7 = addElement "RAG1"       750 rag1       dg6

  in dg7
