{-
  SiRNA.hs — RNA interference pathway: RISC loading, target degradation

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Non-Coding RNAs as Signals and Middleware

  siRNAs and the RNAi pathway are formalized as a typed exception-handling
  system.  The RISC complex acts as a pattern-matching exception handler
  where sequence complementarity determines the match.
-}

module SiRNA
  ( RNAiException(..)
  , RISCComplex(..)
  , SilencingOutcome(..)
  , dicerCleave
  , selectGuideStrand
  , loadRISC
  , riscScan
  , riscMatch
  , processRNAi
  , scanTranscriptome
  ) where

import RNAControl

-- | An RNAi exception: the trigger dsRNA and its processing products
data RNAiException = RNAiException
  { rnaiTrigger  :: [Nucleotide]    -- ^ Original dsRNA trigger
  , rnaiFragments :: [([Nucleotide], [Nucleotide])]  -- ^ Dicer products (sense, antisense)
  } deriving (Show, Eq)

-- | The RISC complex loaded with a guide strand
data RISCComplex = RISCComplex
  { riscGuide     :: [Nucleotide]  -- ^ The loaded guide strand
  , riscTolerance :: Int           -- ^ Mismatch tolerance (0=siRNA, >0=miRNA-like)
  , riscSource    :: String        -- ^ Source annotation (e.g., "siRNA", "miRNA")
  } deriving (Show, Eq)

-- | Outcome of RISC-mediated silencing
data SilencingOutcome = SilencingOutcome
  { soTarget  :: String      -- ^ Name of the targeted mRNA
  , soResult  :: RISCResult  -- ^ Cleave, Repress, or Pass
  , soMismatches :: Int      -- ^ Number of mismatches found
  } deriving (Show, Eq)

-- | Simulate Dicer cleavage of dsRNA into ~21nt siRNA duplexes.
--   Dicer processes from the ends, producing duplexes with 2nt 3' overhangs.
dicerCleave :: [Nucleotide]  -- ^ Sense strand of dsRNA
            -> [Nucleotide]  -- ^ Antisense strand of dsRNA
            -> [([Nucleotide], [Nucleotide])]  -- ^ List of siRNA duplexes
dicerCleave sense antisense =
  let fragLen = 21
      senseLen = length sense
      nFrags = senseLen `div` fragLen
      makeFragment i =
        let start = i * fragLen
            sFrag = take fragLen (drop start sense)
            aFrag = take fragLen (drop start antisense)
        in (sFrag, aFrag)
  in if senseLen < fragLen
     then [(sense, antisense)]  -- Too short to cleave, return as-is
     else map makeFragment [0 .. nFrags - 1]

-- | Select the guide strand from an siRNA duplex.
--   The strand with the less stable 5' end is typically selected as guide.
--   Here we use a simplified thermodynamic asymmetry rule.
selectGuideStrand :: ([Nucleotide], [Nucleotide]) -> [Nucleotide]
selectGuideStrand (sense, antisense) =
  let -- Simplified: score first 4 bases for AU content (less stable)
      auContent xs = length (filter (\n -> n == A || n == U) (take 4 xs))
      senseAU = auContent sense
      antiAU  = auContent antisense
  in if antiAU >= senseAU
     then antisense  -- Antisense has weaker 5' end -> selected as guide
     else sense

-- | Load a guide strand into the RISC complex.
loadRISC :: [Nucleotide]  -- ^ Guide strand
         -> Int           -- ^ Mismatch tolerance
         -> String        -- ^ Source annotation
         -> RISCComplex
loadRISC guide tol src = RISCComplex
  { riscGuide     = guide
  , riscTolerance = tol
  , riscSource    = src
  }

-- | Scan a single target mRNA against a loaded RISC complex.
--   Returns the best match result.
riscScan :: RISCComplex      -- ^ Loaded RISC
         -> [Nucleotide]     -- ^ Target mRNA sequence
         -> RISCResult
riscScan risc target =
  let guide = riscGuide risc
      tol   = riscTolerance risc
      gLen  = length guide
      tLen  = length target
  in if tLen < gLen
     then Pass
     else
       let -- Slide guide along target, check each position
           positions = [0 .. tLen - gLen]
           checkPos i =
             let window = take gLen (drop i target)
                 rc     = reverseComplement guide
                 dist   = hammingDistance rc window
             in dist
           distances = map checkPos positions
           bestDist  = minimum distances
       in riscMatch guide tol bestDist

-- | Determine RISC result from guide, tolerance, and best distance
riscMatch :: [Nucleotide] -> Int -> Int -> RISCResult
riscMatch _ tolerance dist
  | dist <= tolerance && tolerance == 0 = Cleave
  | dist <= tolerance                   = Repress
  | otherwise                           = Pass

-- | Process a complete RNAi event: from dsRNA trigger through silencing.
processRNAi :: [Nucleotide]           -- ^ Sense strand of trigger dsRNA
            -> [Nucleotide]           -- ^ Antisense strand
            -> [(String, [Nucleotide])]  -- ^ Transcriptome: (name, sequence)
            -> [SilencingOutcome]
processRNAi sense antisense transcriptome =
  let -- Step 1: Dicer cleavage
      fragments = dicerCleave sense antisense
      -- Step 2: Guide strand selection
      guides = map selectGuideStrand fragments
      -- Step 3: Load into RISC (tolerance=0 for siRNA)
      riscs = map (\g -> loadRISC g 0 "siRNA") guides
      -- Step 4: Scan transcriptome
      outcomes = concatMap (\r -> scanTranscriptome r transcriptome) riscs
  in outcomes

-- | Scan an entire transcriptome with a loaded RISC complex.
scanTranscriptome :: RISCComplex
                  -> [(String, [Nucleotide])]
                  -> [SilencingOutcome]
scanTranscriptome risc transcriptome =
  let scanOne (name, seq') =
        let result = riscScan risc seq'
            guide  = riscGuide risc
            gLen   = length guide
            tLen   = length seq'
            bestDist = if tLen < gLen
                       then gLen  -- No match possible
                       else minimum [ hammingDistance
                                        (reverseComplement guide)
                                        (take gLen (drop i seq'))
                                    | i <- [0 .. tLen - gLen] ]
        in SilencingOutcome name result bestDist
  in map scanOne transcriptome
