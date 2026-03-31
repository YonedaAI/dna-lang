{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : ExpressionControl
-- Description : Expression level algebra, feedback loops, GRN simulation
-- Copyright   : (c) Matthew Long, YonedaAI Research Collective, 2026
-- License     : BSD-3-Clause
--
-- Gene regulatory network simulation, feedback loop dynamics,
-- and Boolean gene circuit evaluation.

module ExpressionControl
  ( Gene(..)
  , Edge(..)
  , EdgeType(..)
  , GRN(..)
  , mkGene
  , mkEdge
  , mkGRN
  , updateGene
  , stepGRN
  , simulateGRN
  , simulateToggle
  , simulateRepressilator
  , simulateFFL
  , hillFunction
  , negativeFeedback
  , findSteadyState
  , grnInfo
  ) where

-- | A gene in the regulatory network
data Gene = Gene
  { geneName  :: String
  , geneLevel :: Double     -- ^ Current expression level (0.0 to max)
  } deriving (Show, Eq)

-- | Edge type: activation or repression
data EdgeType = Activation | Repression
  deriving (Show, Eq)

-- | A regulatory edge (morphism) in the GRN category
data Edge = Edge
  { edgeFrom   :: Int       -- ^ Index of source gene
  , edgeTo     :: Int       -- ^ Index of target gene
  , edgeType   :: EdgeType  -- ^ Activation or repression
  , edgeWeight :: Double    -- ^ Strength of the regulatory effect
  , edgeHill   :: Double    -- ^ Hill coefficient for the interaction
  , edgeK      :: Double    -- ^ Half-max constant (K in Hill function)
  } deriving (Show, Eq)

-- | Gene regulatory network
data GRN = GRN
  { grnGenes :: [Gene]
  , grnEdges :: [Edge]
  } deriving (Show)

-- | Create a gene with a name and initial expression level
mkGene :: String -> Double -> Gene
mkGene = Gene

-- | Create a regulatory edge with default Hill parameters
mkEdge :: Int -> Int -> EdgeType -> Double -> Edge
mkEdge from to etype w = Edge
  { edgeFrom   = from
  , edgeTo     = to
  , edgeType   = etype
  , edgeWeight = w
  , edgeHill   = 2.0
  , edgeK      = 0.5
  }

-- | Create a GRN from genes and edges
mkGRN :: [Gene] -> [Edge] -> GRN
mkGRN = GRN

-- | Hill function: f(x) = x^n / (K^n + x^n)
hillFunction :: Double -> Double -> Double -> Double
hillFunction n k x
  | x <= 0    = 0.0
  | otherwise  = (x ** n) / (k ** n + x ** n)

-- | Update a single gene based on its incoming regulatory edges
updateGene :: [Gene] -> [Edge] -> Int -> Double -> Double -> Gene
updateGene genes edges idx alpha delta =
  let incomingEdges = filter (\e -> edgeTo e == idx) edges
      currentGene   = genes !! idx
      currentLevel  = geneLevel currentGene
      -- Sum contributions from all regulators
      regulation    = sum $ map (edgeContribution genes) incomingEdges
      -- Production rate depends on regulation
      production    = alpha * (if null incomingEdges
                               then 0.5  -- basal
                               else max 0.0 (min 1.0 regulation))
      -- Degradation
      degradation   = delta * currentLevel
      -- Euler step
      dt            = 0.1
      newLevel      = max 0.0 (currentLevel + dt * (production - degradation))
  in currentGene { geneLevel = newLevel }

-- | Compute the regulatory contribution of a single edge
edgeContribution :: [Gene] -> Edge -> Double
edgeContribution genes edge =
  let sourceLevel = geneLevel (genes !! edgeFrom edge)
      h           = hillFunction (edgeHill edge) (edgeK edge) sourceLevel
      w           = edgeWeight edge
  in case edgeType edge of
       Activation -> w * h
       Repression -> w * (1.0 - h)

-- | Step the entire GRN forward by one time step
stepGRN :: GRN -> Double -> Double -> GRN
stepGRN grn alpha delta =
  let genes' = [ updateGene (grnGenes grn) (grnEdges grn) i alpha delta
               | i <- [0 .. length (grnGenes grn) - 1]
               ]
  in grn { grnGenes = genes' }

-- | Simulate a GRN for a given number of steps
simulateGRN :: GRN -> Int -> Double -> Double -> [GRN]
simulateGRN grn steps alpha delta =
  take steps $ iterate (\g -> stepGRN g alpha delta) grn

-- | Simulate a toggle switch (two mutually repressing genes)
-- Returns list of (gene1_level, gene2_level) pairs
simulateToggle :: Double -> Double -> Int -> [(Double, Double)]
simulateToggle g1Init g2Init steps =
  let alpha = 5.0
      n     = 2.0
      delta = 1.0
      dt    = 0.1
      step (g1, g2) =
        let g1' = g1 + dt * (alpha / (1.0 + g2 ** n) - delta * g1)
            g2' = g2 + dt * (alpha / (1.0 + g1 ** n) - delta * g2)
        in (max 0.0 g1', max 0.0 g2')
  in take steps $ iterate step (g1Init, g2Init)

-- | Simulate a repressilator (three genes in cyclic repression)
-- Returns list of (gene1, gene2, gene3) triples
simulateRepressilator :: Double -> Double -> Double -> Int -> [(Double, Double, Double)]
simulateRepressilator g1Init g2Init g3Init steps =
  let alpha = 5.0
      n     = 2.5
      delta = 1.0
      dt    = 0.05
      step (g1, g2, g3) =
        let g1' = g1 + dt * (alpha / (1.0 + g3 ** n) - delta * g1)
            g2' = g2 + dt * (alpha / (1.0 + g1 ** n) - delta * g2)
            g3' = g3 + dt * (alpha / (1.0 + g2 ** n) - delta * g3)
        in (max 0.0 g1', max 0.0 g2', max 0.0 g3')
  in take steps $ iterate step (g1Init, g2Init, g3Init)

-- | Simulate a coherent feed-forward loop (C1-FFL)
-- X activates Y, X activates Z, Y activates Z
-- Returns list of (X, Y, Z) triples
simulateFFL :: Double -> Int -> [(Double, Double, Double)]
simulateFFL xSignal steps =
  let alpha = 5.0
      n     = 2.0
      delta = 1.0
      dt    = 0.1
      step (_, y, z) =
        let x'  = xSignal  -- X is an external input signal
            y'  = y + dt * (alpha * hillFunction n 0.5 x' - delta * y)
            -- Z requires both X AND Y (AND-gate logic)
            z'  = z + dt * (alpha * hillFunction n 0.5 x' *
                            hillFunction n 0.5 y - delta * z)
        in (x', max 0.0 y', max 0.0 z')
  in take steps $ iterate step (xSignal, 0.0, 0.0)

-- | Negative feedback: converges to setpoint / (1 + gain)
negativeFeedback :: Double -> Double -> Int -> [Double]
negativeFeedback input gain steps =
  let dt = 0.1
      step level =
        let error = input - level
            adjustment = gain * error
        in level + dt * adjustment
  in take steps $ iterate step 0.0

-- | Find the steady state of a simple feedback system
findSteadyState :: Double -> Double -> Double
findSteadyState input gain = input / (1.0 + gain)

-- | Get a human-readable description of a GRN
grnInfo :: GRN -> String
grnInfo grn =
  "GRN with " ++ show (length (grnGenes grn)) ++ " genes and " ++
  show (length (grnEdges grn)) ++ " edges:\n" ++
  concatMap (\g -> "  " ++ geneName g ++ " = " ++
             show (geneLevel g) ++ "\n") (grnGenes grn) ++
  concatMap (\e -> "  " ++ show (edgeFrom e) ++ " -[" ++
             show (edgeType e) ++ ", w=" ++ show (edgeWeight e) ++
             "]-> " ++ show (edgeTo e) ++ "\n") (grnEdges grn)
