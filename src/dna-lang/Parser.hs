{-
  Parser.hs — Simple Parser for DNA-Lang Programs

  Copyright (c) 2026 Matthew Long
  YonedaAI Research Collective

  A hand-written recursive descent parser for DNA-Lang syntax.
  Parses program blocks, declarations, constraints, and actions.
  No external dependencies beyond base.
-}

module Parser (
    parseProgram,
    ParseError(..)
) where

import Types
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

-- ══════════════════════════════════════════════════════════════════
-- Parse Error
-- ══════════════════════════════════════════════════════════════════

data ParseError = ParseError
    { peMessage  :: String
    , peRemaining :: String
    }
    deriving (Show)

-- ══════════════════════════════════════════════════════════════════
-- Parser Monad (simple state-based parser)
-- ══════════════════════════════════════════════════════════════════

newtype Parser a = Parser { runParser :: String -> Either ParseError (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of
        Left err     -> Left err
        Right (a, s') -> Right (f a, s')

instance Applicative Parser where
    pure a = Parser $ \s -> Right (a, s)
    (Parser pf) <*> (Parser pa) = Parser $ \s -> case pf s of
        Left err      -> Left err
        Right (f, s') -> case pa s' of
            Left err       -> Left err
            Right (a, s'') -> Right (f a, s'')

instance Monad Parser where
    (Parser pa) >>= f = Parser $ \s -> case pa s of
        Left err     -> Left err
        Right (a, s') -> runParser (f a) s'

failParse :: String -> Parser a
failParse msg = Parser $ \s -> Left (ParseError msg (take 40 s))

-- ══════════════════════════════════════════════════════════════════
-- Basic Combinators
-- ══════════════════════════════════════════════════════════════════

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred' = Parser $ \s -> case s of
    (c:cs) | pred' c -> Right (c, cs)
    _                -> Left (ParseError "unexpected character" (take 40 s))

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = pure []
string (c:cs) = do
    _ <- char c
    _ <- string cs
    pure (c:cs)

spaces :: Parser ()
spaces = Parser $ \s -> Right ((), dropWhile isSpace s)

lexeme :: Parser a -> Parser a
lexeme p = do
    a <- p
    _ <- spaces
    pure a

keyword :: String -> Parser String
keyword kw = lexeme (string kw)

identifier :: Parser String
identifier = lexeme $ Parser $ \s ->
    let (name, rest) = span (\c -> isAlphaNum c || c == '_') s
    in if null name
       then Left (ParseError "expected identifier" (take 40 s))
       else Right (name, rest)

-- Try a parser; if it fails, backtrack
try' :: Parser a -> Parser (Maybe a)
try' (Parser p) = Parser $ \s -> case p s of
    Left _       -> Right (Nothing, s)
    Right (a, s') -> Right (Just a, s')

optional' :: Parser a -> Parser ()
optional' p = do
    _ <- try' p
    pure ()

many' :: Parser a -> Parser [a]
many' p = do
    ma <- try' p
    case ma of
        Nothing -> pure []
        Just a  -> do
            rest <- many' p
            pure (a : rest)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep' = do
    ma <- try' p
    case ma of
        Nothing -> pure []
        Just a  -> do
            rest <- many' (sep' >> p)
            pure (a : rest)

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
    _ <- open
    a <- p
    _ <- close
    pure a

-- ══════════════════════════════════════════════════════════════════
-- Expression Parser
-- ══════════════════════════════════════════════════════════════════

parseExpr :: Parser Expr
parseExpr = do
    e <- parseAtom
    parseExprTail e

parseExprTail :: Expr -> Parser Expr
parseExprTail e = do
    mc <- try' (char '.')
    case mc of
        Just _ -> do
            _ <- spaces
            field <- identifier
            parseExprTail (EField e field)
        Nothing -> pure e

parseAtom :: Parser Expr
parseAtom = do
    _ <- spaces
    Parser $ \s -> case s of
        ('"':_)               -> runParser parseStringLit s
        ('[':_)               -> runParser parseListExpr s
        ('{':_)               -> runParser parseRecordExpr s
        (c:_) | isDigit c     -> runParser parseNumberLit s
        (c:_) | isAlpha c || c == '_' -> runParser parseVarOrApp s
        _                     -> Left (ParseError "expected expression" (take 40 s))

parseStringLit :: Parser Expr
parseStringLit = lexeme $ do
    _ <- char '"'
    cs <- many' (satisfy (/= '"'))
    _ <- char '"'
    pure (EStringLit cs)

parseNumberLit :: Parser Expr
parseNumberLit = lexeme $ Parser $ \s ->
    let (digits, rest) = span isDigit s
    in case rest of
        ('.':rest2) ->
            let (frac, rest3) = span isDigit rest2
            in Right (EFloatLit (read (digits ++ "." ++ frac)), dropWhile isSpace rest3)
        _ -> Right (EIntLit (read digits), dropWhile isSpace rest)

parseListExpr :: Parser Expr
parseListExpr = do
    _ <- lexeme (char '[')
    es <- sepBy parseExpr (lexeme (char ','))
    _ <- lexeme (char ']')
    pure (EList es)

parseRecordExpr :: Parser Expr
parseRecordExpr = do
    _ <- lexeme (char '{')
    fields <- sepBy parseField (lexeme (char ','))
    _ <- lexeme (char '}')
    pure (ERecord fields)
  where
    parseField = do
        name <- identifier
        _ <- lexeme (char '=')
        val <- parseExpr
        pure (name, val)

parseVarOrApp :: Parser Expr
parseVarOrApp = do
    name <- identifier
    -- Check for true/false
    case name of
        "true"  -> pure (EBoolLit True)
        "false" -> pure (EBoolLit False)
        _       -> do
            mp <- try' (lexeme (char '('))
            case mp of
                Nothing -> pure (EVar name)
                Just _  -> do
                    args <- sepBy parseExpr (lexeme (char ','))
                    _ <- lexeme (char ')')
                    pure (EApp name args)

-- ══════════════════════════════════════════════════════════════════
-- Declaration Parser
-- ══════════════════════════════════════════════════════════════════

parseDecl :: Parser Decl
parseDecl = do
    _ <- spaces
    kw <- identifier
    case kw of
        "target"       -> parseTargetDecl
        "sequence"     -> parseSequenceDecl
        "guide"        -> parseGuideDecl
        "edit"         -> parseEditDecl
        "vector"       -> parseVectorDecl
        "cell_line"    -> parseCellLineDecl
        "assay"        -> parseAssayDecl
        "model_result" -> parseModelResultDecl
        "construct"    -> parseConstructDecl
        "candidate"    -> parseCandidateDecl
        "require"      -> parseRequireDecl
        "let"          -> parseLetDecl
        _              -> failParse ("unknown declaration: " ++ kw)

parseTargetDecl :: Parser Decl
parseTargetDecl = do
    name <- identifier
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DTarget name e)

parseSequenceDecl :: Parser Decl
parseSequenceDecl = do
    name <- identifier
    _ <- lexeme (char ':')
    kind <- parseSeqKind
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DSequence name kind e)

parseSeqKind :: Parser SeqKind
parseSeqKind = do
    kw <- identifier
    case kw of
        "DNA"     -> pure DNA
        "RNA"     -> pure RNA
        "Protein" -> pure Protein
        _         -> failParse ("unknown sequence kind: " ++ kw)

parseGuideDecl :: Parser Decl
parseGuideDecl = do
    name <- identifier
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DGuideRNA name e)

parseEditDecl :: Parser Decl
parseEditDecl = do
    name <- identifier
    _ <- lexeme (char ':')
    kind <- parseEditKind
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DEdit name kind e)

parseEditKind :: Parser EditKind
parseEditKind = do
    kw <- identifier
    case kw of
        "Knockout"  -> pure Knockout
        "BaseEdit"  -> pure BaseEdit
        "PrimeEdit" -> pure PrimeEdit
        "Insertion"  -> pure Insertion
        _            -> failParse ("unknown edit kind: " ++ kw)

parseVectorDecl :: Parser Decl
parseVectorDecl = do
    name <- identifier
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DVector name e)

parseCellLineDecl :: Parser Decl
parseCellLineDecl = do
    name <- identifier
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DCellLine name e)

parseAssayDecl :: Parser Decl
parseAssayDecl = do
    name <- identifier
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DAssay name e)

parseModelResultDecl :: Parser Decl
parseModelResultDecl = do
    name <- identifier
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DModelResult name e)

parseConstructDecl :: Parser Decl
parseConstructDecl = do
    name <- identifier
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DConstruct name e)

parseCandidateDecl :: Parser Decl
parseCandidateDecl = do
    name <- identifier
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DCandidate name e)

parseRequireDecl :: Parser Decl
parseRequireDecl = do
    c <- parseConstraint
    pure (DRequire c)

parseLetDecl :: Parser Decl
parseLetDecl = do
    name <- identifier
    _ <- lexeme (char ':')
    ty <- parseType
    _ <- lexeme (char '=')
    e <- parseExpr
    pure (DLet name ty e)

-- ══════════════════════════════════════════════════════════════════
-- Constraint Parser
-- ══════════════════════════════════════════════════════════════════

parseConstraint :: Parser Constraint
parseConstraint = do
    c <- parseConstraintAtom
    parseConstraintTail c

parseConstraintTail :: Constraint -> Parser Constraint
parseConstraintTail c = do
    mand <- try' (keyword "AND")
    case mand of
        Just _ -> do
            c2 <- parseConstraintAtom
            parseConstraintTail (CAnd c c2)
        Nothing -> do
            mor <- try' (keyword "OR")
            case mor of
                Just _ -> do
                    c2 <- parseConstraintAtom
                    parseConstraintTail (COr c c2)
                Nothing -> pure c

parseConstraintAtom :: Parser Constraint
parseConstraintAtom = do
    kw <- identifier
    case kw of
        "no_off_targets"  -> pure CNoOffTarget
        "specificity"     -> do
            _ <- lexeme (string ">=")
            e <- parseAtom
            case e of
                EFloatLit v -> pure (CSpecificity v)
                _           -> failParse "expected float literal for specificity"
        "efficiency"      -> do
            _ <- lexeme (string ">=")
            e <- parseAtom
            case e of
                EFloatLit v -> pure (CEfficiency v)
                _           -> failParse "expected float literal for efficiency"
        "gc_content"      -> do
            _ <- lexeme (string ">=")
            eLo <- parseAtom
            _ <- keyword "AND"
            _ <- keyword "gc_content"
            _ <- lexeme (string "<=")
            eHi <- parseAtom
            case (eLo, eHi) of
                (EFloatLit lo, EFloatLit hi) -> pure (CGCContent lo hi)
                _ -> failParse "expected float literals for gc_content"
        "length"          -> do
            op <- lexeme (Parser $ \s -> let (o, r) = span (\c' -> c' == '>' || c' == '<' || c' == '=') s
                                         in if null o then Left (ParseError "expected operator" (take 40 s))
                                            else Right (o, r))
            e <- parseAtom
            case e of
                EIntLit n -> case op of
                    ">="  -> pure (CMinLength n)
                    "<="  -> pure (CMaxLength n)
                    _     -> failParse ("unexpected constraint operator: " ++ op)
                _ -> failParse "expected integer literal for length"
        _                 -> failParse ("unknown constraint: " ++ kw)

-- ══════════════════════════════════════════════════════════════════
-- Type Parser
-- ══════════════════════════════════════════════════════════════════

parseType :: Parser DNAType
parseType = do
    kw <- identifier
    case kw of
        "ProteinCode" -> pure TProteinCode
        "Regulator"   -> pure TRegulator
        "RNAControl"  -> pure TRNAControl
        "Structure"   -> pure TStructure
        "Repeat"      -> pure TRepeat
        "State"       -> pure TState
        "Program"     -> pure TProgram
        "Target"      -> pure TTarget
        "Sequence"    -> do
            _ <- lexeme (char '<')
            k <- parseSeqKind
            _ <- lexeme (char '>')
            pure (TSequence k)
        "GuideRNA"    -> pure TGuideRNA
        "Edit"        -> do
            _ <- lexeme (char '<')
            k <- parseEditKind
            _ <- lexeme (char '>')
            pure (TEdit k)
        "Vector"      -> pure TVector
        "CellLine"    -> pure TCellLine
        "Assay"       -> pure TAssay
        "Phenotype"   -> pure TPhenotype
        "Candidate"   -> pure TCandidate
        "Risk"        -> do
            _ <- lexeme (char '<')
            k <- parseRiskKind
            _ <- lexeme (char '>')
            pure (TRisk k)
        "Evidence"    -> pure TEvidence
        "Workflow"    -> pure TWorkflow
        "Genome"      -> pure TGenome
        "String"      -> pure TString
        "Int"         -> pure TInt
        "Float"       -> pure TFloat
        "Bool"        -> pure TBool
        _             -> failParse ("unknown type: " ++ kw)

parseRiskKind :: Parser RiskKind
parseRiskKind = do
    kw <- identifier
    case kw of
        "OffTarget"       -> pure OffTarget
        "Toxicity"        -> pure Toxicity
        "Immunogenicity"  -> pure Immunogenicity
        _                 -> failParse ("unknown risk kind: " ++ kw)

-- ══════════════════════════════════════════════════════════════════
-- Action Parser
-- ══════════════════════════════════════════════════════════════════

parseAction :: Parser Action
parseAction = do
    _ <- spaces
    kw <- identifier
    case kw of
        "run"      -> do
            name <- identifier
            mp <- try' (lexeme (char '('))
            case mp of
                Nothing -> pure (ARun name [])
                Just _  -> do
                    args <- sepBy parseExpr (lexeme (char ','))
                    _ <- lexeme (char ')')
                    pure (ARun name args)
        "collect"  -> do
            name <- identifier
            _ <- lexeme (char '=')
            e <- parseExpr
            pure (ACollect name e)
        "validate" -> do
            e <- parseExpr
            pure (AValidate e)
        "log"      -> do
            e <- parseExpr
            pure (ALog e)
        "return"   -> do
            e <- parseExpr
            pure (AReturn e)
        _          -> failParse ("unknown action: " ++ kw)

-- ══════════════════════════════════════════════════════════════════
-- Program Parser
-- ══════════════════════════════════════════════════════════════════

parseProgramBlock :: Parser Program
parseProgramBlock = do
    _ <- spaces
    _ <- keyword "program"
    name <- identifier
    _ <- lexeme (char '{')
    items <- many' parseProgramItem
    _ <- lexeme (char '}')
    let (decls, actions) = partitionItems items
    pure (Program name decls actions)

data ProgramItem = PDecl Decl | PAction Action

partitionItems :: [ProgramItem] -> ([Decl], [Action])
partitionItems [] = ([], [])
partitionItems (PDecl d : rest)   = let (ds, as) = partitionItems rest in (d:ds, as)
partitionItems (PAction a : rest) = let (ds, as) = partitionItems rest in (ds, a:as)

parseProgramItem :: Parser ProgramItem
parseProgramItem = do
    _ <- spaces
    -- Peek at the next identifier to decide
    Parser $ \s ->
        let (word, _) = span (\c -> isAlphaNum c || c == '_') (dropWhile isSpace s)
        in case word of
            "run"      -> runParser (fmap PAction parseAction) s
            "collect"  -> runParser (fmap PAction parseAction) s
            "validate" -> runParser (fmap PAction parseAction) s
            "log"      -> runParser (fmap PAction parseAction) s
            "return"   -> runParser (fmap PAction parseAction) s
            _          -> runParser (fmap PDecl parseDecl) s

-- ══════════════════════════════════════════════════════════════════
-- Top-level Parse Function
-- ══════════════════════════════════════════════════════════════════

parseProgram :: String -> Either ParseError Program
parseProgram input = case runParser parseProgramBlock input of
    Left err     -> Left err
    Right (p, _) -> Right p
