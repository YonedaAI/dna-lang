{-
  Differentiation.hs -- Waddington landscape, cell fate decisions,
                        stem cell -> terminal cell paths, apoptosis

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Developmental Programs as Orchestration
-}

module Differentiation
  ( Cell(..)
  , CellFateState(..)
  , Potency(..)
  , ApoptosisStage(..)
  , WaddingtonNode(..)
  , WaddingtonLandscape(..)
  , decideFate
  , specifyCell
  , determineCell
  , differentiateCell
  , triggerApoptosis
  , executeApoptosis
  , stemCellDivide
  , asymmetricDivide
  , buildWaddingtonLandscape
  , simulateWaddington
  , hematopoieticLandscape
  , showCell
  , showLandscape
  , showDifferentiationPath
  ) where

-- | Potency levels for stem cells
data Potency
  = Totipotent    -- can become any cell type including extraembryonic
  | Pluripotent   -- can become any embryonic cell type
  | Multipotent   -- can become cells within one lineage
  | Unipotent     -- can become one cell type only
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Cell fate state: progressive type narrowing
data CellFateState
  = Stem Potency         -- stem cell with given potency
  | Progenitor String    -- lineage-restricted progenitor
  | Determined String    -- committed to a specific fate
  | Terminal String      -- terminally differentiated
  | Apoptotic            -- undergoing programmed cell death
  deriving (Show, Eq)

-- | A cell with fate state and metadata
data Cell = Cell
  { cellId      :: String
  , cellState   :: CellFateState
  , cellSignals :: [String]    -- signals received
  , cellAge     :: Int         -- division count
  } deriving (Show, Eq)

-- | Stages of apoptosis
data ApoptosisStage
  = SignalReceived     -- death signal received
  | Committed          -- pro-apoptotic > anti-apoptotic
  | Executing          -- caspase cascade active
  | Fragmented         -- apoptotic bodies formed
  | Recycled           -- phagocytes have cleaned up
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | A node in the Waddington landscape
data WaddingtonNode = WaddingtonNode
  { nodeName     :: String
  , nodeState    :: CellFateState
  , nodeChildren :: [WaddingtonNode]   -- possible fates downstream
  , nodePotential :: Double            -- quasi-potential (height in landscape)
  } deriving (Show)

-- | The full Waddington landscape
data WaddingtonLandscape = WaddingtonLandscape
  { landscapeName :: String
  , landscapeRoot :: WaddingtonNode
  } deriving (Show)

-- | Make a cell-fate decision based on received signals
decideFate :: Cell -> [a] -> Cell
decideFate cell [] = cell
decideFate cell _signals = case cellState cell of
  Stem _potency    -> specifyCell cell "default_lineage"
  Progenitor _lin  -> determineCell cell "default_fate"
  Determined _fate -> differentiateCell cell
  Terminal _       -> cell   -- already fully differentiated
  Apoptotic        -> cell   -- dead cells don't respond

-- | Specify a stem cell toward a lineage (competence -> specification)
specifyCell :: Cell -> String -> Cell
specifyCell cell lineage = cell
  { cellState   = Progenitor lineage
  , cellSignals = cellSignals cell ++ ["specified:" ++ lineage]
  }

-- | Determine a progenitor cell to a specific fate (specification -> determination)
determineCell :: Cell -> String -> Cell
determineCell cell fate = cell
  { cellState   = Determined fate
  , cellSignals = cellSignals cell ++ ["determined:" ++ fate]
  }

-- | Differentiate a determined cell to terminal state
differentiateCell :: Cell -> Cell
differentiateCell cell = case cellState cell of
  Determined fate -> cell
    { cellState   = Terminal fate
    , cellSignals = cellSignals cell ++ ["differentiated"]
    }
  _ -> cell  -- can only differentiate from Determined

-- | Trigger apoptosis on a cell
triggerApoptosis :: Cell -> String -> Cell
triggerApoptosis cell reason = cell
  { cellState   = Apoptotic
  , cellSignals = cellSignals cell ++ ["apoptosis:" ++ reason]
  }

-- | Execute the full apoptosis program, returning stages
executeApoptosis :: Cell -> [(ApoptosisStage, String)]
executeApoptosis cell =
  [ (SignalReceived, cellId cell ++ ": death signal received")
  , (Committed,     cellId cell ++ ": Bax/Bak overwhelm Bcl-2")
  , (Executing,     cellId cell ++ ": caspase cascade activated")
  , (Fragmented,    cellId cell ++ ": cell fragmented into apoptotic bodies")
  , (Recycled,      cellId cell ++ ": phagocytes recycled resources")
  ]

-- | Symmetric stem cell division (self-renewal)
stemCellDivide :: Cell -> (Cell, Cell)
stemCellDivide cell =
  ( cell { cellId = cellId cell ++ "a", cellAge = cellAge cell + 1 }
  , cell { cellId = cellId cell ++ "b", cellAge = cellAge cell + 1 }
  )

-- | Asymmetric division: one stem cell + one progenitor
asymmetricDivide :: Cell -> String -> (Cell, Cell)
asymmetricDivide cell lineage =
  ( cell { cellId = cellId cell ++ "s", cellAge = cellAge cell + 1 }
  , (specifyCell cell lineage) { cellId = cellId cell ++ "d"
                                , cellAge = cellAge cell + 1 }
  )

-- | Build a Waddington landscape tree
buildWaddingtonLandscape :: String -> WaddingtonNode -> WaddingtonLandscape
buildWaddingtonLandscape = WaddingtonLandscape

-- | A model hematopoietic (blood cell) Waddington landscape
hematopoieticLandscape :: WaddingtonLandscape
hematopoieticLandscape = buildWaddingtonLandscape "Hematopoietic" root
  where
    root = WaddingtonNode "HSC" (Stem Multipotent) [myeloid, lymphoid] 1.0

    myeloid = WaddingtonNode "CMP" (Progenitor "Myeloid")
      [erythrocyte, megakaryocyte, granulocyte, monocyte] 0.7

    lymphoid = WaddingtonNode "CLP" (Progenitor "Lymphoid")
      [tCell, bCell, nkCell] 0.7

    erythrocyte  = WaddingtonNode "Erythrocyte" (Terminal "Erythrocyte") [] 0.0
    megakaryocyte = WaddingtonNode "Megakaryocyte" (Terminal "Megakaryocyte") [] 0.0
    granulocyte  = WaddingtonNode "Granulocyte" (Terminal "Granulocyte") [] 0.0
    monocyte     = WaddingtonNode "Monocyte" (Terminal "Monocyte") [] 0.0
    tCell        = WaddingtonNode "T-Cell" (Terminal "T-Cell") [] 0.0
    bCell        = WaddingtonNode "B-Cell" (Terminal "B-Cell") [] 0.0
    nkCell       = WaddingtonNode "NK-Cell" (Terminal "NK-Cell") [] 0.0

-- | Simulate a cell rolling down the Waddington landscape
--   Given a sequence of branch choices (0-indexed), trace the path
simulateWaddington :: WaddingtonLandscape -> [Int] -> [WaddingtonNode]
simulateWaddington landscape choices = go (landscapeRoot landscape) choices
  where
    go node [] = [node]
    go node (c:cs) =
      let children = nodeChildren node
      in if null children
         then [node]  -- reached a terminal fate
         else let idx = c `mod` length children
                  child = children !! idx
              in node : go child cs

-- | Show a cell's state
showCell :: Cell -> String
showCell c = "Cell[" ++ cellId c ++ "] state=" ++ showFate (cellState c)
          ++ " age=" ++ show (cellAge c)
          ++ (if null (cellSignals c) then ""
              else " signals=" ++ show (cellSignals c))

showFate :: CellFateState -> String
showFate (Stem p)       = "Stem(" ++ show p ++ ")"
showFate (Progenitor l) = "Progenitor(" ++ l ++ ")"
showFate (Determined f) = "Determined(" ++ f ++ ")"
showFate (Terminal f)   = "Terminal(" ++ f ++ ")"
showFate Apoptotic      = "Apoptotic"

-- | Display the Waddington landscape as a tree
showLandscape :: WaddingtonLandscape -> String
showLandscape landscape = "=== Waddington Landscape: " ++ landscapeName landscape
                       ++ " ===\n" ++ showTree 0 (landscapeRoot landscape)
  where
    showTree indent node =
      replicate (indent * 2) ' '
      ++ nodeName node ++ " [" ++ showFate (nodeState node)
      ++ ", potential=" ++ showF1 (nodePotential node) ++ "]\n"
      ++ concatMap (showTree (indent + 1)) (nodeChildren node)

    showF1 x = let whole = floor x :: Int
                   frac  = round ((x - fromIntegral whole) * 10) :: Int
               in show whole ++ "." ++ show frac

-- | Show a differentiation path through the landscape
showDifferentiationPath :: [WaddingtonNode] -> String
showDifferentiationPath [] = "(empty path)"
showDifferentiationPath nodes =
  "Differentiation Path:\n"
  ++ concatMap (\(i, n) ->
       "  Step " ++ show i ++ ": " ++ nodeName n
       ++ " [" ++ showFate (nodeState n) ++ "]\n")
     (zip [(0::Int)..] nodes)
  ++ "  Final fate: " ++ nodeName (last nodes) ++ "\n"
