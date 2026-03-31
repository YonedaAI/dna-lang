{-
  RegulatoryNetwork.hs -- Master regulator cascades, feed-forward loops,
                          regulatory graph traversal

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Developmental Programs as Orchestration
-}

module RegulatoryNetwork
  ( TFNode(..)
  , RegEdge(..)
  , RegNetwork(..)
  , TFActivation(..)
  , LoopMotif(..)
  , makeNetwork
  , myogenicCascade
  , neuralCascade
  , addNode
  , addEdge
  , regNodes
  , regEdges
  , topologicalSort
  , bootSequence
  , findFeedForwardLoops
  , findMasterRegulators
  , activationOrder
  , showNetwork
  , showBootSequence
  ) where

-- | A transcription factor node in the regulatory network
data TFNode = TFNode
  { tfName        :: String
  , tfDescription :: String
  , tfIsMaster    :: Bool     -- is this a master regulator?
  } deriving (Show, Eq)

-- | An edge in the regulatory network (activation dependency)
data RegEdge = RegEdge
  { edgeFrom     :: String   -- upstream TF name
  , edgeTo       :: String   -- downstream TF name
  , edgeType     :: String   -- "activates", "represses", "co-activates"
  } deriving (Show, Eq)

-- | The regulatory network (DAG)
data RegNetwork = RegNetwork
  { networkName  :: String
  , networkNodes :: [TFNode]
  , networkEdges :: [RegEdge]
  } deriving (Show)

-- | Convenience accessors for Program.hs compatibility
regNodes :: RegNetwork -> [TFNode]
regNodes = networkNodes

regEdges :: RegNetwork -> [RegEdge]
regEdges = networkEdges

-- | An activation event in the boot sequence
data TFActivation = TFActivation
  { activatedTF    :: String
  , activationStep :: Int
  , prerequisites  :: [String]
  } deriving (Show)

-- | A feed-forward loop motif (X -> Y -> Z, X -> Z)
data LoopMotif = LoopMotif
  { loopX :: String
  , loopY :: String
  , loopZ :: String
  } deriving (Show)

-- | Create a regulatory network
makeNetwork :: String -> [TFNode] -> [RegEdge] -> RegNetwork
makeNetwork = RegNetwork

-- | Add a node to the network
addNode :: RegNetwork -> TFNode -> RegNetwork
addNode net node = net { networkNodes = networkNodes net ++ [node] }

-- | Add an edge to the network
addEdge :: RegNetwork -> RegEdge -> RegNetwork
addEdge net edge = net { networkEdges = networkEdges net ++ [edge] }

-- | The myogenic regulatory cascade
myogenicCascade :: RegNetwork
myogenicCascade = makeNetwork "Myogenic Cascade" nodes edges
  where
    nodes =
      [ TFNode "Pax3"     "Paired box 3 - somite marker"           True
      , TFNode "Pax7"     "Paired box 7 - satellite cell marker"   True
      , TFNode "Myf5"     "Myogenic factor 5"                      False
      , TFNode "MyoD"     "Myoblast determination protein"         False
      , TFNode "Myogenin" "Myogenin - commitment factor"           False
      , TFNode "MRF4"     "Muscle regulatory factor 4"             False
      , TFNode "Myosin"   "Myosin heavy chain - terminal marker"   False
      ]
    edges =
      [ RegEdge "Pax3"     "Myf5"     "activates"
      , RegEdge "Pax7"     "Myf5"     "activates"
      , RegEdge "Pax3"     "MyoD"     "activates"
      , RegEdge "Myf5"     "MyoD"     "co-activates"
      , RegEdge "MyoD"     "Myogenin" "activates"
      , RegEdge "MyoD"     "MyoD"     "auto-activates"
      , RegEdge "Myogenin" "MRF4"     "activates"
      , RegEdge "Myogenin" "Myosin"   "activates"
      , RegEdge "MRF4"     "Myosin"   "co-activates"
      ]

-- | A neural differentiation cascade
neuralCascade :: RegNetwork
neuralCascade = makeNetwork "Neural Cascade" nodes edges
  where
    nodes =
      [ TFNode "Sox2"    "SRY-box 2 - neural stem cell marker"   True
      , TFNode "Pax6"    "Paired box 6 - neural progenitor"      True
      , TFNode "Neurogenin" "Neurogenin - proneural factor"       False
      , TFNode "NeuroD"  "Neuronal differentiation factor"        False
      , TFNode "Tbr2"    "T-box brain 2 - intermediate prog."    False
      , TFNode "Tbr1"    "T-box brain 1 - postmitotic neuron"    False
      , TFNode "NeuN"    "Neuronal nuclear antigen - terminal"    False
      ]
    edges =
      [ RegEdge "Sox2"       "Pax6"       "activates"
      , RegEdge "Pax6"       "Neurogenin" "activates"
      , RegEdge "Pax6"       "Tbr2"       "activates"
      , RegEdge "Neurogenin" "NeuroD"     "activates"
      , RegEdge "Neurogenin" "Tbr2"       "co-activates"
      , RegEdge "Tbr2"       "Tbr1"       "activates"
      , RegEdge "NeuroD"     "Tbr1"       "co-activates"
      , RegEdge "Tbr1"       "NeuN"       "activates"
      ]

-- | Topological sort of the regulatory network (boot order)
--   Returns nodes in activation order (dependencies first)
topologicalSort :: [TFNode] -> [RegEdge] -> [TFActivation]
topologicalSort nodes edges = go [] [] (map tfName nodes) 0
  where
    -- Find all prerequisites for a given node
    prereqsOf name = [edgeFrom e | e <- edges,
                       edgeTo e == name,
                       edgeFrom e /= name]  -- exclude self-loops

    -- Check if all prereqs of a node are in the 'done' set
    ready done name = all (`elem` done) (prereqsOf name)

    go acc done [] _ = reverse acc
    go acc done remaining step =
      let (canActivate, waiting) = partition' (ready done) remaining
      in if null canActivate
         then -- Remaining nodes have unresolvable deps (cycle or missing)
              reverse acc ++ map (\n -> TFActivation n step (prereqsOf n)) waiting
         else let newActivations = map (\n -> TFActivation n step (prereqsOf n))
                                       canActivate
                  done' = done ++ canActivate
              in go (reverse newActivations ++ acc) done' waiting (step + 1)

    -- Simple partition (no Data.List needed)
    partition' _ [] = ([], [])
    partition' p (x:xs) =
      let (yes, no) = partition' p xs
      in if p x then (x:yes, no) else (yes, x:no)

-- | Compute the boot sequence for a regulatory network
bootSequence :: RegNetwork -> [TFActivation]
bootSequence net = topologicalSort (networkNodes net) (networkEdges net)

-- | Find all feed-forward loop motifs in the network
findFeedForwardLoops :: RegNetwork -> [LoopMotif]
findFeedForwardLoops net =
  [ LoopMotif x y z
  | RegEdge x y _ <- networkEdges net
  , RegEdge y' z _ <- networkEdges net
  , y == y'
  , RegEdge x' z' _ <- networkEdges net
  , x == x'
  , z == z'
  , x /= y
  , y /= z
  , x /= z
  ]

-- | Find master regulators (nodes with no incoming edges, or marked as master)
findMasterRegulators :: RegNetwork -> [TFNode]
findMasterRegulators net =
  filter (\n -> tfIsMaster n || hasNoIncoming (tfName n)) (networkNodes net)
  where
    hasNoIncoming name = null [e | e <- networkEdges net,
                                    edgeTo e == name,
                                    edgeFrom e /= name]

-- | Get the activation order as a simple list of names
activationOrder :: RegNetwork -> [String]
activationOrder net = map activatedTF (bootSequence net)

-- | Display the network
showNetwork :: RegNetwork -> String
showNetwork net = header ++ nodeSection ++ edgeSection ++ motifSection
  where
    header = "=== Regulatory Network: " ++ networkName net ++ " ===\n\n"
    nodeSection = "Transcription Factors:\n"
               ++ concatMap showNode (networkNodes net) ++ "\n"
    showNode n = "  " ++ (if tfIsMaster n then "[MASTER] " else "         ")
              ++ tfName n ++ " - " ++ tfDescription n ++ "\n"
    edgeSection = "Regulatory Edges:\n"
               ++ concatMap showEdge (networkEdges net) ++ "\n"
    showEdge e = "  " ++ edgeFrom e ++ " --[" ++ edgeType e ++ "]--> "
              ++ edgeTo e ++ "\n"
    ffls = findFeedForwardLoops net
    motifSection = "Feed-Forward Loops: " ++ show (length ffls) ++ "\n"
                ++ concatMap showFFL ffls
    showFFL m = "  " ++ loopX m ++ " -> " ++ loopY m ++ " -> " ++ loopZ m
             ++ " (with " ++ loopX m ++ " -> " ++ loopZ m ++ ")\n"

-- | Display the boot sequence
showBootSequence :: RegNetwork -> String
showBootSequence net = "Boot Sequence for: " ++ networkName net ++ "\n"
                    ++ concatMap showStep (bootSequence net)
  where
    showStep a = "  Step " ++ show (activationStep a) ++ ": "
              ++ activatedTF a
              ++ (if null (prerequisites a) then " (root)"
                  else " <- requires " ++ showList' (prerequisites a))
              ++ "\n"
    showList' [] = "none"
    showList' xs = foldr1 (\a b -> a ++ ", " ++ b) xs
