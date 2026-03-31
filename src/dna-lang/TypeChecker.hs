{-
  TypeChecker.hs — Type Checker for DNA-Lang

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  Validates DNA-Lang programs against biological type constraints.
  Implements typing rules, refinement type checking, and biological
  constraint validation.
-}

module TypeChecker (
    typeCheck,
    TypeError(..),
    TypeResult
) where

import Types
import qualified Data.Map as Map

-- ══════════════════════════════════════════════════════════════════
-- Type Errors
-- ══════════════════════════════════════════════════════════════════

data TypeError
    = UnboundVariable String
    | TypeMismatch DNAType DNAType String   -- expected, actual, context
    | ConstraintViolation String
    | InvalidEdit String
    | InvalidTarget String
    | MissingRequiredField String String    -- declaration, field
    | UnsatisfiableConstraint Constraint
    | MultipleErrors [TypeError]
    deriving (Show)

type TypeResult a = Either TypeError a

-- ══════════════════════════════════════════════════════════════════
-- Built-in Function Signatures
-- ══════════════════════════════════════════════════════════════════

builtinFunctions :: Map.Map String (DNAType, [DNAType])
builtinFunctions = Map.fromList
    [ ("predict_structure",  (TStructure, [TSequence Protein]))
    , ("rank_guides",        (TList TGuideRNA, [TTarget]))
    , ("predict_off_target", (TRisk OffTarget, [TGuideRNA]))
    , ("propose_variants",   (TList (TSequence Protein), [TSequence Protein]))
    , ("design_guide",       (TGuideRNA, [TTarget, TSequence DNA]))
    , ("knockout",           (TEdit Knockout, [TTarget, TGuideRNA]))
    , ("base_edit",          (TEdit BaseEdit, [TTarget, TGuideRNA, TSequence DNA]))
    , ("prime_edit",         (TEdit PrimeEdit, [TTarget, TGuideRNA, TSequence DNA]))
    , ("insert_payload",     (TEdit Insertion, [TTarget, TGuideRNA, TSequence DNA]))
    , ("assemble_construct", (TVector, [TEdit Knockout, TSequence DNA]))
    , ("run_assay",          (TPhenotype, [TAssay, TCellLine]))
    , ("validate_safety",    (TList (TRisk OffTarget), [TEdit Knockout]))
    , ("generate_report",    (TEvidence, [TAssay, TEdit Knockout]))
    , ("lookup_sequence",    (TSequence DNA, [TTarget]))
    , ("select_cell_line",   (TCellLine, [TString]))
    , ("design_assay",       (TAssay, [TEdit Knockout, TCellLine]))
    ]

-- ══════════════════════════════════════════════════════════════════
-- Expression Type Inference
-- ══════════════════════════════════════════════════════════════════

inferExpr :: TypeEnv -> Expr -> TypeResult DNAType
inferExpr _ (EStringLit _)  = Right TString
inferExpr _ (EIntLit _)     = Right TInt
inferExpr _ (EFloatLit _)   = Right TFloat
inferExpr _ (EBoolLit _)    = Right TBool
inferExpr env (EVar name)   =
    case lookupEnv name env of
        Just ty -> Right ty
        Nothing -> Left (UnboundVariable name)
inferExpr env (EList [])    = Right (TList TUnit)
inferExpr env (EList (e:_)) = do
    ty <- inferExpr env e
    Right (TList ty)
inferExpr env (ERecord fields) = do
    -- Records are typed as their declared context type
    -- For now, return a generic workflow type
    mapM_ (\(_, v) -> inferExpr env v) fields
    Right TWorkflow
inferExpr env (EApp fname args) = do
    case Map.lookup fname builtinFunctions of
        Just (retTy, paramTys) -> do
            argTys <- mapM (inferExpr env) args
            checkArgs fname paramTys argTys
            Right retTy
        Nothing ->
            case lookupEnv fname env of
                Just (TFunction paramTys retTy) -> do
                    argTys <- mapM (inferExpr env) args
                    checkArgs fname paramTys argTys
                    Right retTy
                Just ty -> Right ty  -- Treat as a constant
                Nothing -> Left (UnboundVariable fname)
inferExpr env (EField e _) = do
    _ <- inferExpr env e
    -- Field access returns a generic type; real implementation
    -- would look up the field in the record type
    Right TString
inferExpr env (EBinOp _ a b) = do
    ta <- inferExpr env a
    tb <- inferExpr env b
    Right TBool  -- Comparison operators return bool
inferExpr env (ETyped e ty) = do
    actualTy <- inferExpr env e
    if isSubtype actualTy ty || actualTy == TString || actualTy == TWorkflow
        then Right ty
        else Left (TypeMismatch ty actualTy "type annotation")

checkArgs :: String -> [DNAType] -> [DNAType] -> TypeResult ()
checkArgs fname expected actual
    | length expected /= length actual =
        Left (TypeMismatch (TFunction expected TUnit) (TFunction actual TUnit)
              ("argument count mismatch in " ++ fname))
    | otherwise = mapM_ checkOne (zip3 [1..] expected actual)
  where
    checkOne :: (Int, DNAType, DNAType) -> TypeResult ()
    checkOne (i, exp', act) =
        if isSubtype act exp' || act == TString || exp' == TString
        then Right ()
        else Left (TypeMismatch exp' act
                   ("argument " ++ show i ++ " of " ++ fname))

    zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
    zip3 (a:as) (b:bs) (c:cs) = (a, b, c) : zip3 as bs cs
    zip3 _ _ _ = []

-- ══════════════════════════════════════════════════════════════════
-- Declaration Type Checking
-- ══════════════════════════════════════════════════════════════════

checkDecl :: TypeEnv -> Decl -> TypeResult TypeEnv
checkDecl env (DTarget name expr) = do
    _ <- inferExpr env expr
    Right (extendEnv name TTarget env)

checkDecl env (DSequence name kind expr) = do
    _ <- inferExpr env expr
    Right (extendEnv name (TSequence kind) env)

checkDecl env (DGuideRNA name expr) = do
    ty <- inferExpr env expr
    case ty of
        TGuideRNA -> Right (extendEnv name TGuideRNA env)
        TList TGuideRNA -> Right (extendEnv name (TList TGuideRNA) env)
        _ -> Right (extendEnv name TGuideRNA env)  -- Allow flexible guide construction

checkDecl env (DEdit name kind expr) = do
    ty <- inferExpr env expr
    -- Verify the expression produces a compatible edit type
    case ty of
        TEdit actualKind ->
            if actualKind == kind || isSubtype (TEdit actualKind) (TEdit kind)
            then Right (extendEnv name (TEdit kind) env)
            else Left (InvalidEdit $ "expected Edit<" ++ show kind ++ ">, got Edit<" ++ show actualKind ++ ">")
        _ -> Right (extendEnv name (TEdit kind) env)  -- Allow construction from expressions

checkDecl env (DVector name expr) = do
    _ <- inferExpr env expr
    Right (extendEnv name TVector env)

checkDecl env (DCellLine name expr) = do
    _ <- inferExpr env expr
    Right (extendEnv name TCellLine env)

checkDecl env (DAssay name expr) = do
    _ <- inferExpr env expr
    Right (extendEnv name TAssay env)

checkDecl env (DModelResult name expr) = do
    _ <- inferExpr env expr
    Right (extendEnv name TWorkflow env)

checkDecl env (DConstruct name expr) = do
    _ <- inferExpr env expr
    Right (extendEnv name TVector env)

checkDecl env (DCandidate name expr) = do
    _ <- inferExpr env expr
    Right (extendEnv name TCandidate env)

checkDecl env (DLet name ty expr) = do
    actualTy <- inferExpr env expr
    if isSubtype actualTy ty || actualTy == TString || actualTy == TWorkflow
        then Right (extendEnv name ty env)
        else Left (TypeMismatch ty actualTy ("let binding " ++ name))

checkDecl env (DRequire constraint) = do
    validateConstraint env constraint
    Right env

-- ══════════════════════════════════════════════════════════════════
-- Constraint Validation
-- ══════════════════════════════════════════════════════════════════

validateConstraint :: TypeEnv -> Constraint -> TypeResult ()
validateConstraint _ CTrue = Right ()
validateConstraint _ CNoOffTarget = Right ()
validateConstraint _ (CMinLength n)
    | n >= 0    = Right ()
    | otherwise = Left (ConstraintViolation "minimum length must be non-negative")
validateConstraint _ (CMaxLength n)
    | n > 0     = Right ()
    | otherwise = Left (ConstraintViolation "maximum length must be positive")
validateConstraint _ (CGCContent lo hi)
    | lo >= 0 && hi <= 1.0 && lo <= hi = Right ()
    | otherwise = Left (ConstraintViolation "GC content must be in [0,1] with lo <= hi")
validateConstraint _ (CSpecificity s)
    | s >= 0 && s <= 1.0 = Right ()
    | otherwise = Left (ConstraintViolation "specificity must be in [0,1]")
validateConstraint _ (CEfficiency e)
    | e >= 0 && e <= 1.0 = Right ()
    | otherwise = Left (ConstraintViolation "efficiency must be in [0,1]")
validateConstraint _ (CMaxRisk _ v)
    | v >= 0 = Right ()
    | otherwise = Left (ConstraintViolation "max risk must be non-negative")
validateConstraint _ (CValidPAM p)
    | not (null p) = Right ()
    | otherwise = Left (ConstraintViolation "PAM sequence must not be empty")
validateConstraint _ (CExpression lo hi)
    | lo <= hi  = Right ()
    | otherwise = Left (ConstraintViolation "expression range lo must be <= hi")
validateConstraint env (CAnd a b) = do
    validateConstraint env a
    validateConstraint env b
validateConstraint env (COr a b) = do
    validateConstraint env a
    validateConstraint env b
validateConstraint env (CNot c) = validateConstraint env c

-- ══════════════════════════════════════════════════════════════════
-- Action Type Checking
-- ══════════════════════════════════════════════════════════════════

checkAction :: TypeEnv -> Action -> TypeResult TypeEnv
checkAction env (ARun name args) = do
    mapM_ (inferExpr env) args
    -- run actions don't introduce bindings but may have side effects
    Right env

checkAction env (ACollect name expr) = do
    ty <- inferExpr env expr
    Right (extendEnv name ty env)

checkAction env (AValidate expr) = do
    _ <- inferExpr env expr
    Right env

checkAction env (ALog expr) = do
    _ <- inferExpr env expr
    Right env

checkAction env (AReturn expr) = do
    _ <- inferExpr env expr
    Right env

-- ══════════════════════════════════════════════════════════════════
-- Program Type Checking
-- ══════════════════════════════════════════════════════════════════

typeCheck :: Program -> TypeResult TypeEnv
typeCheck (Program _ decls actions) = do
    env1 <- foldDecls emptyEnv decls
    env2 <- foldActions env1 actions
    Right env2
  where
    foldDecls env [] = Right env
    foldDecls env (d:ds) = do
        env' <- checkDecl env d
        foldDecls env' ds

    foldActions env [] = Right env
    foldActions env (a:as) = do
        env' <- checkAction env a
        foldActions env' as
