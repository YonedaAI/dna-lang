{-
  Program.hs -- Core developmental program type and stage transitions

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Developmental Programs as Orchestration
-}

module Program
  ( DevStage(..)
  , Program(..)
  , SignalState(..)
  , Signal(..)
  , SignalType(..)
  , initProgram
  , advanceStage
  , stageOps
  , isValidOp
  , runDevelopment
  , showProgram
  ) where

import BodyPlan
import RegulatoryNetwork
import Differentiation

-- | Developmental stages, ordered by progression
data DevStage
  = Cleavage
  | Gastrulation
  | Organogenesis
  | Maturation
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Signal types that cells can receive
data SignalType
  = InductiveSignal
  | DetermSignal
  | DiffSignal
  | ApoptoticSignal
  | MitogenicSignal
  deriving (Show, Eq)

-- | A signal with identity and type
data Signal = Signal
  { signalName :: String
  , signalType :: SignalType
  , signalStrength :: Double
  } deriving (Show, Eq)

-- | Global signaling state
data SignalState = SignalState
  { activeSignals   :: [Signal]
  , morphogenLevels :: [(String, Double)]
  } deriving (Show)

-- | The core developmental program type, parameterized by stage
data Program d = Program
  { hoxCluster         :: HoxCluster
  , morphogens         :: [MorphogenField]
  , cellPopulation     :: [Cell]
  , regulatoryNet      :: RegNetwork
  , signalingState     :: SignalState
  , developmental_stage :: d
  }

-- | Operations permitted at each stage
stageOps :: DevStage -> [String]
stageOps Cleavage      = ["divide", "compact"]
stageOps Gastrulation  = ["invaginate", "migrate", "specify"]
stageOps Organogenesis = ["differentiate", "morphogenize", "apoptose"]
stageOps Maturation    = ["grow", "remodel"]

-- | Check whether an operation is valid at the current stage
isValidOp :: DevStage -> String -> Bool
isValidOp stage op = op `elem` stageOps stage

-- | Advance to the next developmental stage
advanceStage :: DevStage -> Maybe DevStage
advanceStage Maturation = Nothing
advanceStage s          = Just (succ s)

-- | Initialize a developmental program from a Hox cluster
initProgram :: HoxCluster -> [MorphogenField] -> RegNetwork -> Program DevStage
initProgram hox morphs regNet = Program
  { hoxCluster         = hox
  , morphogens         = morphs
  , cellPopulation     = [makeZygote]
  , regulatoryNet      = regNet
  , signalingState     = SignalState [] []
  , developmental_stage = Cleavage
  }

-- | Create the initial zygote cell
makeZygote :: Cell
makeZygote = Cell
  { cellId      = "zygote"
  , cellState   = Stem Totipotent
  , cellSignals = []
  , cellAge     = 0
  }

-- | Run one step of development
stepDevelopment :: Program DevStage -> Program DevStage
stepDevelopment prog =
  let stage = developmental_stage prog
      cells = cellPopulation prog
      morphs = morphogens prog
      signals = activeSignals (signalingState prog)
      -- Apply morphogen interpretation to cells
      cells' = map (\c -> decideFate c signals) cells
      -- Expand population during cleavage
      cells'' = case stage of
        Cleavage -> concatMap divideCell cells'
        _        -> cells'
  in prog { cellPopulation = cells'' }
  where
    divideCell c = case cellState c of
      Stem _ -> [ c { cellId = cellId c ++ "a", cellAge = cellAge c + 1 }
                , c { cellId = cellId c ++ "b", cellAge = cellAge c + 1 }
                ]
      _      -> [c]

-- | Run the full developmental program through all stages
runDevelopment :: Program DevStage -> [Program DevStage]
runDevelopment prog =
  let prog' = stepDevelopment prog
  in case advanceStage (developmental_stage prog') of
       Nothing    -> [prog']
       Just next  -> prog' : runDevelopment (prog' { developmental_stage = next })

-- | Display a program summary
showProgram :: Program DevStage -> String
showProgram prog = joinLines
  [ "=== Developmental Program ==="
  , "Stage: " ++ show (developmental_stage prog)
  , "Hox cluster: " ++ clusterName (hoxCluster prog)
    ++ " (" ++ show (length (hoxGenes (hoxCluster prog))) ++ " genes)"
  , "Morphogen fields: " ++ show (length (morphogens prog))
  , "Cell population: " ++ show (length (cellPopulation prog)) ++ " cells"
  , "Regulatory nodes: " ++ show (length (regNodes (regulatoryNet prog)))
  , "Valid operations: " ++ show (stageOps (developmental_stage prog))
  , ""
  , "--- Cell Census ---"
  ] ++ joinLines (map showCellBrief (cellPopulation prog))

showCellBrief :: Cell -> String
showCellBrief c = "  " ++ cellId c ++ " : " ++ showState (cellState c)
  where
    showState (Stem p)        = "Stem(" ++ show p ++ ")"
    showState (Progenitor l)  = "Progenitor(" ++ l ++ ")"
    showState (Determined f)  = "Determined(" ++ f ++ ")"
    showState (Terminal f)    = "Terminal(" ++ f ++ ")"
    showState Apoptotic       = "Apoptotic"

joinLines :: [String] -> String
joinLines [] = ""
joinLines [x] = x
joinLines (x:xs) = x ++ "\n" ++ joinLines xs
