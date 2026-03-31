{-
  Types.hs — Unified Type System for DNA-Lang

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  The complete type system for DNA-Lang, encoding all seven biological
  types from Papers I-VII plus biotech workflow types.
-}

module Types (
    DNAType(..),
    SeqKind(..),
    EditKind(..),
    RiskKind(..),
    Constraint(..),
    Expr(..),
    Decl(..),
    Action(..),
    Program(..),
    Artifact(..),
    ArtifactKind(..),
    TypeEnv,
    emptyEnv,
    extendEnv,
    lookupEnv,
    isSubtype,
    showType,
    showExpr,
    showConstraint
) where

import qualified Data.Map as Map

-- ══════════════════════════════════════════════════════════════════
-- Core Types
-- ══════════════════════════════════════════════════════════════════

-- | The seven biological types (Papers I-VII) plus biotech workflow types.
data DNAType
    -- Paper I: Coding Sequences as Executable Functions
    = TProteinCode
    -- Paper II: Regulatory Sequences as Control Flow
    | TRegulator
    -- Paper III: Non-coding RNA as Signals/Middleware
    | TRNAControl
    -- Paper IV: Structural DNA as Memory Layout
    | TStructure
    -- Paper V: Repetitive Elements as Self-Modifying Code
    | TRepeat
    -- Paper VI: Epigenetic Marks as Runtime State
    | TState
    -- Paper VII: Developmental Programs as Orchestration
    | TProgram
    -- Biotech workflow types
    | TTarget
    | TSequence SeqKind
    | TGuideRNA
    | TEdit EditKind
    | TVector
    | TCellLine
    | TAssay
    | TPhenotype
    | TCandidate
    | TRisk RiskKind
    | TEvidence
    | TWorkflow
    -- Refinement type: base type + constraint
    | TRefinement DNAType Constraint
    -- Composite genome type
    | TGenome
    -- Function type
    | TFunction [DNAType] DNAType
    -- List type
    | TList DNAType
    -- Unit type (for actions with no return)
    | TUnit
    -- String/numeric literals
    | TString
    | TInt
    | TFloat
    | TBool
    deriving (Eq, Show)

data SeqKind = DNA | RNA | Protein
    deriving (Eq, Show, Ord)

data EditKind = Knockout | BaseEdit | PrimeEdit | Insertion
    deriving (Eq, Show, Ord)

data RiskKind = OffTarget | Toxicity | Immunogenicity
    deriving (Eq, Show, Ord)

-- ══════════════════════════════════════════════════════════════════
-- Constraints
-- ══════════════════════════════════════════════════════════════════

data Constraint
    = CMinLength Int
    | CMaxLength Int
    | CGCContent Double Double     -- min, max GC fraction
    | CNoOffTarget                 -- zero predicted off-targets
    | CSpecificity Double          -- minimum specificity score
    | CEfficiency Double           -- minimum efficiency score
    | CMaxRisk RiskKind Double     -- max acceptable risk score
    | CValidPAM String             -- PAM sequence constraint
    | CExpression Double Double    -- min, max expression level
    | CAnd Constraint Constraint
    | COr Constraint Constraint
    | CNot Constraint
    | CTrue
    deriving (Eq, Show)

-- ══════════════════════════════════════════════════════════════════
-- Expressions and AST
-- ══════════════════════════════════════════════════════════════════

data Expr
    = EVar String
    | EStringLit String
    | EIntLit Int
    | EFloatLit Double
    | EBoolLit Bool
    | EList [Expr]
    | ERecord [(String, Expr)]
    | EApp String [Expr]          -- function application
    | EField Expr String          -- field access: expr.field
    | EBinOp String Expr Expr     -- binary operation
    | ETyped Expr DNAType         -- type annotation
    deriving (Eq, Show)

data Decl
    = DTarget String Expr                    -- target gene = expr
    | DSequence String SeqKind Expr          -- sequence name : kind = expr
    | DGuideRNA String Expr                  -- guide name = design(target)
    | DEdit String EditKind Expr             -- edit name : kind = expr
    | DVector String Expr                    -- vector name = expr
    | DCellLine String Expr                  -- cell_line name = expr
    | DAssay String Expr                     -- assay name = expr
    | DModelResult String Expr               -- model_result name = expr
    | DConstruct String Expr                 -- construct name = expr
    | DCandidate String Expr                 -- candidate name = expr
    | DLet String DNAType Expr               -- let binding with type
    | DRequire Constraint                    -- require constraint
    deriving (Eq, Show)

data Action
    = ARun String [Expr]          -- run action(args)
    | ACollect String Expr        -- collect name = expr
    | AValidate Expr              -- validate expr
    | ALog Expr                   -- log expr
    | AReturn Expr                -- return expr
    deriving (Eq, Show)

data Program = Program
    { progName   :: String
    , progDecls  :: [Decl]
    , progActions :: [Action]
    }
    deriving (Eq, Show)

-- ══════════════════════════════════════════════════════════════════
-- Compiler Artifacts
-- ══════════════════════════════════════════════════════════════════

data ArtifactKind = DesignArtifact | AIArtifact | ExperimentArtifact | ComplianceArtifact
    deriving (Eq, Show, Ord)

data Artifact = Artifact
    { artKind    :: ArtifactKind
    , artContent :: [(String, String)]   -- key-value pairs
    }
    deriving (Eq, Show)

-- ══════════════════════════════════════════════════════════════════
-- Type Environment
-- ══════════════════════════════════════════════════════════════════

type TypeEnv = Map.Map String DNAType

emptyEnv :: TypeEnv
emptyEnv = Map.empty

extendEnv :: String -> DNAType -> TypeEnv -> TypeEnv
extendEnv = Map.insert

lookupEnv :: String -> TypeEnv -> Maybe DNAType
lookupEnv = Map.lookup

-- ══════════════════════════════════════════════════════════════════
-- Subtyping
-- ══════════════════════════════════════════════════════════════════

-- | Subtyping relation: isSubtype a b means a <: b
isSubtype :: DNAType -> DNAType -> Bool
isSubtype a b | a == b = True
-- Refinement types are subtypes of their base
isSubtype (TRefinement base _) b = isSubtype base b
-- Sequence subtypes
isSubtype (TSequence DNA) (TSequence RNA) = False
isSubtype (TSequence _) (TSequence _) = False
-- Edit subtypes: all edits are subtypes of Knockout (most general)
isSubtype (TEdit BaseEdit) (TEdit Knockout) = True
isSubtype (TEdit PrimeEdit) (TEdit Knockout) = True
isSubtype (TEdit Insertion) (TEdit Knockout) = True
-- Candidate is subtype of Evidence
isSubtype TCandidate TEvidence = True
-- GuideRNA requires Sequence DNA
isSubtype TGuideRNA (TSequence DNA) = True
-- List covariance
isSubtype (TList a) (TList b) = isSubtype a b
-- Genome contains all biological types
isSubtype TProteinCode TGenome = True
isSubtype TRegulator TGenome = True
isSubtype TRNAControl TGenome = True
isSubtype TStructure TGenome = True
isSubtype TRepeat TGenome = True
isSubtype TState TGenome = True
isSubtype TProgram TGenome = True
isSubtype _ _ = False

-- ══════════════════════════════════════════════════════════════════
-- Pretty Printing
-- ══════════════════════════════════════════════════════════════════

showType :: DNAType -> String
showType TProteinCode     = "ProteinCode<T>"
showType TRegulator       = "Regulator<ExpressionLevel>"
showType TRNAControl      = "RNAControl<Process>"
showType TStructure       = "Structure<GenomeLayout>"
showType TRepeat          = "Repeat<SelfModifying>"
showType TState           = "State<Accessibility>"
showType TProgram         = "Program<OrganismDevelopment>"
showType TTarget          = "Target"
showType (TSequence k)    = "Sequence<" ++ show k ++ ">"
showType TGuideRNA        = "GuideRNA"
showType (TEdit k)        = "Edit<" ++ show k ++ ">"
showType TVector          = "Vector"
showType TCellLine        = "CellLine"
showType TAssay           = "Assay"
showType TPhenotype       = "Phenotype"
showType TCandidate       = "Candidate"
showType (TRisk k)        = "Risk<" ++ show k ++ ">"
showType TEvidence        = "Evidence"
showType TWorkflow        = "Workflow"
showType (TRefinement t c) = showType t ++ " { " ++ showConstraint c ++ " }"
showType TGenome          = "Genome"
showType (TFunction args ret) = "(" ++ concatWith ", " (map showType args) ++ ") -> " ++ showType ret
showType (TList t)        = "[" ++ showType t ++ "]"
showType TUnit            = "()"
showType TString          = "String"
showType TInt             = "Int"
showType TFloat           = "Float"
showType TBool            = "Bool"

concatWith :: String -> [String] -> String
concatWith _ []     = ""
concatWith _ [x]    = x
concatWith sep (x:xs) = x ++ sep ++ concatWith sep xs

showConstraint :: Constraint -> String
showConstraint (CMinLength n)    = "length >= " ++ show n
showConstraint (CMaxLength n)    = "length <= " ++ show n
showConstraint (CGCContent lo hi) = show lo ++ " <= GC <= " ++ show hi
showConstraint CNoOffTarget      = "off_targets == 0"
showConstraint (CSpecificity s)  = "specificity >= " ++ show s
showConstraint (CEfficiency e)   = "efficiency >= " ++ show e
showConstraint (CMaxRisk k v)    = "risk(" ++ show k ++ ") <= " ++ show v
showConstraint (CValidPAM p)     = "PAM == " ++ show p
showConstraint (CExpression lo hi) = show lo ++ " <= expression <= " ++ show hi
showConstraint (CAnd a b)        = showConstraint a ++ " AND " ++ showConstraint b
showConstraint (COr a b)         = "(" ++ showConstraint a ++ " OR " ++ showConstraint b ++ ")"
showConstraint (CNot c)          = "NOT (" ++ showConstraint c ++ ")"
showConstraint CTrue             = "true"

showExpr :: Expr -> String
showExpr (EVar s)        = s
showExpr (EStringLit s)  = show s
showExpr (EIntLit n)     = show n
showExpr (EFloatLit d)   = show d
showExpr (EBoolLit b)    = show b
showExpr (EList es)      = "[" ++ concatWith ", " (map showExpr es) ++ "]"
showExpr (ERecord fs)    = "{ " ++ concatWith ", " (map (\(k,v) -> k ++ " = " ++ showExpr v) fs) ++ " }"
showExpr (EApp f args)   = f ++ "(" ++ concatWith ", " (map showExpr args) ++ ")"
showExpr (EField e f)    = showExpr e ++ "." ++ f
showExpr (EBinOp op a b) = showExpr a ++ " " ++ op ++ " " ++ showExpr b
showExpr (ETyped e t)    = showExpr e ++ " : " ++ showType t
