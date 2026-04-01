# DNA-Lang Haskell Source

**42 modules across 8 directories implementing a typed biological programming model**

Each directory corresponds to one research paper and implements the biological data type it formalizes. All modules compile with `ghc` using only `base` and `containers` — no external dependencies.

## Quick Start

```bash
# Run any paper's demonstrations
cd src/coding-sequences && ghc -o main Main.hs && ./main

# Run all 8
for d in src/*/; do (cd "$d" && ghc -o main Main.hs && ./main); done
```

---

## Architecture Overview

```
src/
├── coding-sequences/       Paper I:    ProteinCode<T>           5 modules
├── regulatory-sequences/   Paper II:   Regulator<ExpressionLevel>  5 modules
├── noncoding-rna/          Paper III:  RNAControl<Process>      5 modules
├── structural-dna/         Paper IV:   Structure<GenomeLayout>  5 modules
├── repetitive-elements/    Paper V:    Repeat<SelfModifying>    5 modules
├── epigenetic-marks/       Paper VI:   State<Accessibility>     5 modules
├── developmental-programs/ Paper VII:  Program<OrganismDev>     5 modules
└── dna-lang/               Paper VIII: Full Language             7 modules
```

**Key patterns across all modules:**
- **Parametric types** — `Structure a`, `Regulator a`, `RNAControl a`, `Program s`, `EpigeneticState a`
- **Lattice theory** — `ExpressionLevel`, `ActivityState`, `AccessibilityLevel` with join/meet operations
- **Category theory** — Colimits (lncRNA scaffold assembly), functors (Hox collinearity), natural transformations (tRNA)
- **Monadic composition** — `MethylationMonad`, `EpigeneticState` monad, `Runtime` state monad
- **Refinement types** — `TRefinement DNAType Constraint` for biological validity constraints

---

## Paper I: Coding Sequences — `ProteinCode<T>`

**The central dogma as a typed compilation pipeline: DNA → mRNA → Protein = Source → IR → Binary**

### Modules

| Module | Purpose |
|--------|---------|
| `CodonTable.hs` | Complete 64-codon → 20 amino acid mapping |
| `ProteinCode.hs` | Core types: `Nucleotide`, `GeneStructure`, `ProteinCode`, `MutationType` |
| `Transcription.hs` | DNA → mRNA conversion, splicing, exon selection |
| `Translation.hs` | mRNA → protein, reading frames, start site detection |
| `Main.hs` | 6 demos: transcription, translation, codon table, mutations, splicing, PTMs |

### Key Types

```haskell
data Nucleotide = A | T | G | C
data AminoAcid = Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile
               | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val

data GeneStructure = GeneStructure
  { geneID :: String, exons :: [Exon], introns :: [Intron]
  , promoter :: String, UTR5 :: String, UTR3 :: String }

data ProteinCode = ProteinCode
  { proteinID :: String, sequence :: [AminoAcid], mass :: Double, domains :: [String] }

data MutationType = Silent | Missense | Nonsense | Frameshift

data SplicingContext = Constitutive | Alternative
data PostTranslationalMod = Phosphorylation | Acetylation | Ubiquitination | Glycosylation | SUMOylation
```

### Key Functions

```haskell
codonTable          :: String -> Maybe AminoAcid        -- 64-codon lookup
transcribeSense     :: GeneStructure -> mRNA             -- DNA → mRNA
spliceMRNA          :: mRNA -> SplicingContext -> mRNA   -- context-dependent splicing
translateFromStart  :: String -> [AminoAcid]             -- mRNA → protein
classifyMutation    :: [Nucleotide] -> [Nucleotide] -> MutationType
allReadingFrames    :: String -> [[AminoAcid]]           -- all 3 reading frames
postTranslationalModify :: ProteinCode -> PostTranslationalMod -> ProteinCode
```

---

## Paper II: Regulatory Sequences — `Regulator<ExpressionLevel>`

**Gene expression control as a typed control flow algebra**

### Modules

| Module | Purpose |
|--------|---------|
| `Promoter.hs` | Promoter strength scoring, TATA box, TF binding affinity |
| `Enhancer.hs` | Distance-dependent enhancement, insulator blocking, super-enhancers |
| `Regulator.hs` | Core `Regulator a` type, `ExpressionLevel` lattice, silencers |
| `ExpressionControl.hs` | GRN simulation: toggle switch, repressilator, feed-forward loops |
| `Main.hs` | 8 demos: promoter binding, enhancer effects, lattice ops, gene circuits |

### Key Types

```haskell
data ExpressionLevel = Off | Basal | Induced Double | Max  -- lattice with join/meet

data Promoter = Promoter
  { promoterID :: String, strength :: PromoterStrength
  , tataBox :: TATABox, tfBindingSites :: [TFBindingSite] }

data Enhancer = Enhancer
  { enhancerID :: String, distanceToGene :: Double
  , loopingProbability :: Double, tfBindingSites :: [TFBindingSite] }

data Regulator a = Regulator
  { regPromoter :: Promoter, regEnhancers :: [Enhancer]
  , regSilencers :: [Silencer], regOutput :: a -> ExpressionLevel }

data EdgeType = Activation | Repression
data GRN = GRN { genes :: [Gene], regulatoryEdges :: [Edge] }
```

### Key Functions

```haskell
scorePromoter             :: Promoter -> Double
effectiveEnhancement      :: Enhancer -> Double              -- power-law distance model
combineExpressionLevels   :: ExpressionLevel -> ExpressionLevel -> ExpressionLevel
hillFunction              :: Double -> Double -> Double -> Double  -- concentration, K, n
simulateToggle            :: GRN -> Int -> [(Double, Double)]     -- bistable switch
simulateRepressilator     :: GRN -> Int -> [[Double]]             -- ring oscillator
simulateFFL               :: GRN -> Int -> [[Double]]             -- feed-forward loop
```

---

## Paper III: Non-Coding RNAs — `RNAControl<Process>`

**Signals, interrupts, and middleware for post-transcriptional control**

### Modules

| Module | Purpose |
|--------|---------|
| `RNAControl.hs` | Core type with 6 constructors (MiRNA, SiRNA, LncRNA, TRNA, RRNA, PiRNA) |
| `MicroRNA.hs` | Seed matching, target prediction, cooperative repression |
| `SiRNA.hs` | Dicer cleavage, RISC loading, RNAi pathway, target scanning |
| `LncRNA.hs` | Scaffold assembly (colimit), sponge effects, middleware roles |
| `Main.hs` | 5 demos: miRNA targeting, RNAi silencing, tRNA adaptation, scaffolding, piRNA defense |

### Key Types

```haskell
data RNAControl a = MiRNA a | SiRNA a | LncRNA a | TRNA a | RRNA a | PiRNA a

data RISCComplex = RISCComplex
  { guideStrand :: String, passengerStrand :: String
  , argonaute :: String, loadingState :: Bool }

data ScaffoldSpec = ScaffoldSpec
  { scaffoldID :: String, bindingDomains :: [BindingDomain], topology :: String }

data Complex = Complex
  { complexID :: String, components :: [String], stateID :: Int }

data SilencingOutcome = CleavagePath | MicroRNAPath
```

### Key Functions

```haskell
findSeedMatches      :: String -> String -> [SeedMatch]           -- miRNA target search
cooperativeRepression :: [SeedMatch] -> Double                    -- multi-site repression
dicerCleave          :: String -> (String, String)                -- dsRNA → guide + passenger
riscScan             :: RISCComplex -> [String] -> [String]       -- transcriptome search
assembleComplex      :: ScaffoldSpec -> [String] -> Complex       -- colimit: scaffold assembly
spongeEffect         :: String -> [String] -> Double              -- competitive endogenous RNA
```

---

## Paper IV: Structural DNA — `Structure<GenomeLayout>`

**Genome organization as memory architecture**

### Modules

| Module | Purpose |
|--------|---------|
| `Structure.hs` | Core `Structure a` type, topology, regions, memory tiers, territories |
| `Telomere.hs` | Linear resource consumption, Hayflick limit, telomerase, lifespan simulation |
| `Centromere.hs` | Kinetochore, spindle checkpoint (SAC), chromosome segregation |
| `ChromatinLayout.hs` | TAD modeling, CTCF/cohesin boundaries, chromatin loops, compartments |
| `Main.hs` | 5 demos: telomere shortening, checkpoint barrier, TADs, loop dereferencing, memory hierarchy |

### Key Types

```haskell
data Structure a = Structure
  { topology :: Topology, regions :: [GenomicRegion]
  , compartments :: [String], accessPattern :: a }

data Telomere = Telomere
  { chromosome :: String, repeats :: Int
  , divisionsElapsed :: Int, telomeraseActive :: Bool }

data TelomereState = Healthy | Shortened | Critical | Senescent | Crisis

data TAD = TAD { tadID :: String, start :: Int, end :: Int
               , left_boundary :: TADBoundary, right_boundary :: TADBoundary }

data ChromatinLoop = ChromatinLoop
  { loopID :: String, left_anchor :: LoopAnchor, right_anchor :: LoopAnchor }

data AttachmentState = Unattached | Monotelic | Syntelic | Amphitelic | Merotelic
```

### Key Functions

```haskell
divide             :: Telomere -> Maybe Telomere         -- consumes repeats (linear type)
simulateLifespan   :: Telomere -> [(Int, TelomereState)] -- full lifespan trajectory
evaluateCheckpoint :: SpindleCheckpoint -> Bool           -- SAC pass/fail
segregate          :: Centromere -> (Centromere, Centromere)
nestTADs           :: [TAD] -> TAD                       -- hierarchical organization
dereferenceLoop    :: ChromatinLoop -> GenomicRegion      -- pointer semantics
```

---

## Paper V: Repetitive Elements — `Repeat<SelfModifying>`

**Transposable elements as self-modifying code**

### Modules

| Module | Purpose |
|--------|---------|
| `Repeat.hs` | Core type (DNATransposon, Retrotransposon, SatelliteDNA, ERV), activity states |
| `Transposon.hs` | Cut-paste, copy-paste, TIR recognition, TSD insertion, domestication |
| `SatelliteDNA.hs` | Alpha satellite, telomeric repeats, copy number variation |
| `DynamicGenome.hs` | Genome-level simulation, composition tracking, transposition events |
| `Main.hs` | 7 demos: cut-paste, copy-paste, satellites, ERV decay, domestication, composition, telomere dynamics |

### Key Types

```haskell
data Repeat = DNATransposon { ... } | Retrotransposon { ... }
            | SatelliteDNA { ... }  | ERV { ... }

data ActivityState = Active | Suppressed | Fossilized | Domesticated  -- lattice

data TranspositionMode = CutAndPaste | CopyAndPaste | TargetPrimedRT

data DynamicGenome = DynamicGenome
  { loci :: [RepeatLocus], eventHistory :: [TranspositionEvent], lastUpdate :: Int }

data GenomeComposition = GenomeComposition
  { totalLength :: Int, repeatFraction :: Double
  , transposonFraction :: Double, satelliteFraction :: Double }
```

### Key Functions

```haskell
excise              :: GenomeLocus -> Maybe Repeat                  -- cut
insertAt            :: Repeat -> Int -> Genome -> Genome            -- paste
copyAndPaste        :: Repeat -> Int -> Genome -> Genome            -- retrotransposition
targetPrimedRT      :: Repeat -> Int -> Genome -> Genome            -- LINE insertion
domesticate         :: Repeat -> Repeat                             -- Active → Domesticated
expandRepeat        :: RepeatUnit -> RepeatUnit                     -- CNV expansion
computeGenomeComposition :: DynamicGenome -> GenomeComposition
```

---

## Paper VI: Epigenetic Marks — `State<Accessibility>`

**Chromatin modifications as runtime state and permissions**

### Modules

| Module | Purpose |
|--------|---------|
| `State.hs` | Core `EpigeneticState a` with Functor/Applicative/Monad instances |
| `Methylation.hs` | CpG methylation, DNMT/TET enzymes, CpG islands, `MethylationMonad` |
| `HistoneModification.hs` | Histone marks, writer/eraser/reader enzymes, permission readout |
| `Accessibility.hs` | Chromatin state machine, remodelers (SWI/SNF, ISWI, CHD, INO80), X-inactivation |
| `Main.hs` | 7 demos: methylation, histone cascades, bivalent resolution, inheritance, X-inactivation, remodeling, monad composition |

### Key Types

```haskell
data EpigeneticState a  -- Functor, Applicative, Monad

data AccessibilityLevel = FullyOpen | PartiallyOpen | Bivalent
                        | PartiallyRepressed | FullyRepressed | ConstitutiveHet

data MethylationStatus = Unmethylated | Hemimethylated | FullyMethylated | Hydroxymethylated

data HistoneMark = HistoneMark { position :: HistonePosition, modification :: ModificationType }
data Permission = Execute | ReadWrite | ReadOnly | NoRead | KernelLock

data WriterEnzyme = MLL1_2 | SET1 | EZH2 | SUV39H1 | P300_CBP | DOT1L | SETD2
data EraserEnzyme = JMJD3 | LSD1 | HDAC1_2 | KDM1A
data RemodelerFamily = SWI_SNF | ISWI | CHD | INO80

data MethylationMonad a  -- Monad instance for sequential methylation state transformations
```

### Key Functions

```haskell
-- Methylation
applyDNMT3        :: CpGSite -> CpGSite                    -- de novo methylation
applyDNMT1        :: CpGSite -> CpGSite                    -- maintenance methylation
applyTET          :: CpGSite -> CpGSite                    -- oxidative demethylation
detectCpGIslands  :: String -> [CpGIsland]

-- Histones
writeHistoneMark  :: WriterEnzyme -> HistoneState -> HistoneState
eraseHistoneMark  :: EraserEnzyme -> HistoneState -> HistoneState
combinatorialReadout :: HistoneState -> Permission          -- histone code → permission

-- Chromatin
computeAccessibility :: MethylationStatus -> HistoneState -> ChromatinState
applyRemodeler    :: Remodeler -> ChromatinState -> ChromatinState  -- GC analogy
xInactivate       :: TranscriptionFactor -> XInactivationState     -- process isolation
simulateDivision  :: ChromatinState -> CellDivisionResult          -- inheritance fidelity
```

---

## Paper VII: Developmental Programs — `Program<OrganismDevelopment>`

**Morphogenesis as typed orchestration**

### Modules

| Module | Purpose |
|--------|---------|
| `Program.hs` | Core `Program s` type indexed by `DevStage`, stage-gated operations |
| `BodyPlan.hs` | Hox clusters, collinearity verification, morphogen gradients, French flag model |
| `RegulatoryNetwork.hs` | Regulatory DAGs, topological sort, feed-forward loops, master regulators |
| `Differentiation.hs` | Waddington landscape, cell fate state machine, apoptosis, stem cell division |
| `Main.hs` | 8 demos: Hox collinearity, morphogen gradients, boot sequences, fate decisions, Waddington, apoptosis, stem cells, full orchestration |

### Key Types

```haskell
data DevStage = Cleavage | Gastrulation | Organogenesis | Maturation
data Program s  -- parametric over developmental stage

data HoxCluster = HoxCluster { clusterID :: String, genes :: [HoxGene] }
data MorphogenField = MorphogenField { morphogen :: String, source :: Double, decay :: Double }

data Potency = Totipotent | Pluripotent | Multipotent | Unipotent
data CellFateState = Stem | Progenitor | Determined | Terminal | Apoptotic
data Cell = Cell { cellID :: String, potency :: Potency, fateState :: CellFateState }

data WaddingtonNode = WaddingtonNode { nodeID :: String, potentialField :: Double }
```

### Key Functions

```haskell
verifySpatialCollinearity  :: HoxCluster -> Bool            -- monotone functor check
frenchFlagPartition        :: MorphogenField -> [CellFateZone]  -- threshold interpretation
topologicalSort            :: RegNetwork -> [TFNode]        -- boot sequence ordering
findFeedForwardLoops       :: RegNetwork -> [LoopMotif]
decideFate                 :: Cell -> CellFateState -> Cell  -- type refinement (narrowing)
asymmetricDivide           :: Cell -> (Cell, Cell)           -- stem + progenitor
executeApoptosis           :: Cell -> ApoptosisStage -> Maybe Cell  -- graceful shutdown
simulateWaddington         :: Cell -> WaddingtonLandscape -> CellFateState
runDevelopment             :: Program Cleavage -> Program Maturation  -- full lifecycle
```

---

## Paper VIII: DNA-Lang — The Complete Language

**Parser, type checker, 4-target compiler, and runtime for biological programming**

### Modules

| Module | Purpose |
|--------|---------|
| `Types.hs` | Complete type system: 24+ type constructors, constraints, AST, subtyping |
| `Parser.hs` | Hand-written recursive descent parser for DNA-Lang source text |
| `TypeChecker.hs` | Type inference and biological constraint validation |
| `Compiler.hs` | 4-artifact compiler: Design, AI, Experiment, Compliance |
| `Runtime.hs` | Simulation-mode runtime with 10+ predefined biological functions |
| `Examples.hs` | 3 sample programs: PCSK9_KO, SCD_BaseEdit, FLDL_Activate |
| `Main.hs` | Full pipeline demo: parse → type check → compile → execute |

### The Type System

```haskell
data DNAType
  -- Biological types (Papers I-VII)
  = TProteinCode | TRegulator | TRNAControl | TStructure
  | TRepeat | TState | TProgram
  -- Biotech workflow types
  | TTarget | TSequence SeqKind | TGuideRNA | TEdit EditKind
  | TVector | TCellLine | TAssay | TPhenotype | TCandidate
  | TRisk RiskKind | TEvidence | TWorkflow
  -- Type operators
  | TRefinement DNAType Constraint | TGenome
  | TFunction [DNAType] DNAType | TList DNAType | TUnit
  -- Literals
  | TString | TInt | TFloat | TBool

data SeqKind = DNA | RNA | Protein
data EditKind = Knockout | BaseEdit | PrimeEdit | Insertion
data RiskKind = OffTarget | Toxicity | Immunogenicity

data Constraint
  = CMinLength Int | CMaxLength Int | CGCContent Double Double
  | CNoOffTarget Double | CSpecificity Double | CEfficiency Double
  | CMaxRisk Double | CValidPAM String | CExpression Double
  | CAnd Constraint Constraint | COr Constraint Constraint | CNot Constraint | CTrue
```

### The AST

```haskell
data Expr
  = EVar String | EStringLit String | EIntLit Int | EFloatLit Double
  | EBoolLit Bool | EList [Expr] | ERecord [(String, Expr)]
  | EApp String [Expr] | EField Expr String | EBinOp String Expr Expr
  | ETyped Expr DNAType

data Decl
  = DTarget String Expr | DSequence String SeqKind Expr
  | DGuideRNA String Expr | DEdit String EditKind Expr
  | DVector String Expr | DCellLine String Expr | DAssay String Expr
  | DModelResult String Expr | DConstruct String Expr | DCandidate String Expr
  | DLet String Expr | DRequire Expr

data Action = ARun Expr | ACollect Expr | AValidate Expr | ALog Expr | AReturn Expr

data Program = Program
  { progName :: String, progDecls :: [Decl], progActions :: [Action] }
```

### Compiler Pipeline

```haskell
-- Parser: source text → AST
parseProgram    :: String -> Either ParseError Program

-- Type Checker: AST → validated AST
typeCheck       :: Program -> TypeEnv -> Either TypeError TypeEnv

-- Compiler: AST → 4 artifacts
compile         :: Program -> [Artifact]
compileDesign   :: Program -> Artifact    -- sequences, guides, constructs
compileAI       :: Program -> Artifact    -- predictions, scores, uncertainty
compileExperiment :: Program -> Artifact  -- assay plan, controls, criteria
compileCompliance :: Program -> Artifact  -- provenance, model versions, decision log

-- Runtime: AST → execution results
execute         :: Program -> RuntimeResult
```

### Example Programs

Three complete programs demonstrate the language:

**PCSK9_KO** — Cholesterol reduction via CRISPR knockout
```
program PCSK9_KO {
  target t = gene("PCSK9")
  edit e = knockout(target=t)
  guides g = design_guides(target=t, method=CRISPR_Cas9)
  guides safe_g = filter(g, off_target_score < 0.05)
  construct v = build_vector(cargo=safe_g, delivery=LNP)
  require v.payload_size <= delivery_capacity(LNP)
  assay a = plan_assay(system=hepatocyte_organoid, readouts=[editing_rate, LDL_reduction])
  run a
  collect evidence
}
```

**SCD_BaseEdit** — Sickle cell disease via base editing

**FLDL_Activate** — Familial hypercholesterolemia via CRISPRa gene activation

---

## Building

Every directory compiles independently with no external dependencies:

```bash
cd src/<directory>
ghc -o main Main.hs
./main
```

Requires GHC (any recent version). Uses only `base` and `containers`.

## Author

Matthew Long — The YonedaAI Research Collective — Chicago, IL — 2026
