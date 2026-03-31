{-
  LncRNA.hs — Scaffold assembly and complex formation

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Part of the DNA-Lang project: Non-Coding RNAs as Signals and Middleware

  Long non-coding RNAs are formalized as middleware/scaffolds.
  Scaffold assembly is modeled as a categorical colimit:
  the assembled complex is the minimal object containing all
  binding partners with all specified interactions.
-}

module LncRNA
  ( Complex(..)
  , BindingDomain(..)
  , ScaffoldSpec(..)
  , MiddlewareEffect(..)
  , assembleComplex
  , addPartner
  , removePartner
  , scaffoldBind
  , applyMiddleware
  , classifyRole
  , spongeEffect
  , guideToLocus
  , decoySequester
  , colimitAssembly
  ) where

import RNAControl

-- | A binding domain within a lncRNA scaffold
data BindingDomain = BindingDomain
  { bdName     :: String    -- ^ Domain name (e.g., "5' domain", "3' domain")
  , bdStart    :: Int       -- ^ Start position in lncRNA
  , bdEnd      :: Int       -- ^ End position in lncRNA
  , bdPartner  :: String    -- ^ Name of the binding partner
  } deriving (Show, Eq)

-- | Specification for a scaffold-mediated assembly
data ScaffoldSpec = ScaffoldSpec
  { ssName     :: String           -- ^ Scaffold lncRNA name
  , ssDomains  :: [BindingDomain]  -- ^ Binding domains
  , ssRole     :: LncRNARole       -- ^ Functional role
  } deriving (Show, Eq)

-- | An assembled molecular complex (the colimit)
data Complex = Complex
  { cxName      :: String    -- ^ Complex name
  , cxScaffold  :: String    -- ^ Scaffold lncRNA name
  , cxMembers   :: [String]  -- ^ All member molecules
  , cxComplete  :: Bool      -- ^ Whether all binding sites are occupied
  } deriving (Show, Eq)

-- | Effects produced by lncRNA middleware
data MiddlewareEffect
  = ChromatinModification String  -- ^ Modify chromatin at a locus
  | TranscriptionalActivation String  -- ^ Activate a gene
  | TranscriptionalRepression String  -- ^ Repress a gene
  | MiRNASequestration String    -- ^ Sequester a miRNA (sponge)
  | ProteinSequestration String  -- ^ Sequester a protein (decoy)
  | GuidedRecruitment String String  -- ^ Recruit X to locus Y
  | NoEffect
  deriving (Show, Eq)

-- | Assemble a complex from a scaffold specification.
--   This implements the colimit: the result is the minimal complex
--   containing all partners joined through the scaffold.
assembleComplex :: ScaffoldSpec -> Complex
assembleComplex spec =
  let partners = map bdPartner (ssDomains spec)
      allMembers = ssName spec : partners
  in Complex
       { cxName     = ssName spec ++ "-complex"
       , cxScaffold = ssName spec
       , cxMembers  = allMembers
       , cxComplete = True
       }

-- | Add a binding partner to an existing complex
addPartner :: Complex -> String -> Complex
addPartner cx partner =
  cx { cxMembers = cxMembers cx ++ [partner] }

-- | Remove a binding partner from a complex.
--   If the partner was essential, the complex becomes incomplete.
removePartner :: Complex -> String -> Complex
removePartner cx partner =
  let newMembers = filter (/= partner) (cxMembers cx)
      -- Complex is incomplete if we lost a member
      complete = length newMembers == length (cxMembers cx)
  in cx { cxMembers = newMembers, cxComplete = complete }

-- | Simulate a single binding event between a scaffold domain and its partner
scaffoldBind :: ScaffoldSpec -> String -> Maybe BindingDomain
scaffoldBind spec partnerName =
  case filter (\d -> bdPartner d == partnerName) (ssDomains spec) of
    (d:_) -> Just d
    []    -> Nothing

-- | Apply the middleware effect based on the lncRNA role and assembled complex
applyMiddleware :: ScaffoldSpec -> Complex -> MiddlewareEffect
applyMiddleware spec cx
  | not (cxComplete cx) = NoEffect
  | otherwise = case ssRole spec of
      Scaffold -> ChromatinModification (cxName cx)
      Guide    -> case cxMembers cx of
                    (_scaffold:target:_) -> GuidedRecruitment (cxName cx) target
                    _ -> NoEffect
      Decoy    -> case cxMembers cx of
                    (_scaffold:factor:_) -> ProteinSequestration factor
                    _ -> NoEffect
      Enhancer -> TranscriptionalActivation (cxName cx)
      Sponge   -> case cxMembers cx of
                    (_scaffold:mirna:_) -> MiRNASequestration mirna
                    _ -> NoEffect

-- | Classify the role of a lncRNA based on its binding partners and context
classifyRole :: [String]  -- ^ Binding partner names
             -> Bool      -- ^ Has chromatin-modifying partners?
             -> Bool      -- ^ Has miRNA response elements?
             -> LncRNARole
classifyRole partners hasChromatin hasMREs
  | hasMREs && length partners <= 2 = Sponge
  | hasChromatin && length partners >= 2 = Scaffold
  | hasChromatin && length partners == 1 = Guide
  | not hasChromatin && length partners == 1 = Decoy
  | otherwise = Enhancer

-- | Model the miRNA sponge effect of a lncRNA.
--   Returns the fraction of free miRNA remaining after sequestration.
--
--   freeFraction = max(0, 1 - spongeConc * bindingSites / miRNAConc)
spongeEffect :: Double  -- ^ lncRNA (sponge) concentration
             -> Int     -- ^ Number of miRNA binding sites on the sponge
             -> Double  -- ^ Total miRNA concentration
             -> Double  -- ^ Fraction of miRNA remaining free
spongeEffect spongeConc nSites mirnaConc
  | mirnaConc <= 0 = 0.0
  | otherwise =
      let sequestered = spongeConc * fromIntegral nSites
          freeRatio   = 1.0 - (sequestered / mirnaConc)
      in max 0.0 (min 1.0 freeRatio)

-- | Model a guide lncRNA recruiting a complex to a genomic locus
guideToLocus :: String  -- ^ lncRNA name
             -> String  -- ^ Complex to recruit
             -> String  -- ^ Target locus
             -> MiddlewareEffect
guideToLocus _lnc complex locus = GuidedRecruitment complex locus

-- | Model a decoy lncRNA sequestering a transcription factor
decoySequester :: String  -- ^ lncRNA name
               -> String  -- ^ Transcription factor to sequester
               -> MiddlewareEffect
decoySequester _lnc factor = ProteinSequestration factor

-- | Compute the colimit assembly, verifying all binding constraints.
--   Returns Nothing if the assembly is impossible (missing partners).
colimitAssembly :: ScaffoldSpec
                -> [String]       -- ^ Available molecules
                -> Maybe Complex
colimitAssembly spec available =
  let required = map bdPartner (ssDomains spec)
      allPresent = all (`elem` available) required
  in if allPresent
     then Just (assembleComplex spec)
     else Nothing
