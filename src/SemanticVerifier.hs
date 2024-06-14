module SemanticVerifier
    ( SemanticError(..)
    , semanticVerify
    , mainDetection
    , conflictionCheck
    , variableTypeCheck
    , continueAndBreakCheck
    , identifierReferenceCheck
    ) where

import ErrorHandling
import ExpressionAnalyser (Expression(..))
import SyntaxAnalyser (Syntax(..))
import Tokeniser (Token(..))

data SemanticError = NoMainDefined String Int
                   | IdentifierConfliction String Int String
                   | TypeContradiction String Int String String String String
                   | InappropriateVarType String Int String
                   | LoopFeatureOutsideLoop String Int String
                   | UndefinedIdentifier String Int String
                   | BrokenProgramStructure String Int
                   deriving (Show, Eq)

instance TrackableError SemanticError where
    place (NoMainDefined a b)             = (a, b)
    place (IdentifierConfliction a b _)   = (a, b)
    place (TypeContradiction a b _ _ _ _) = (a, b)
    place (InappropriateVarType a b _)    = (a, b)
    place (LoopFeatureOutsideLoop a b _)  = (a, b)
    place (UndefinedIdentifier a b _)     = (a, b)
    place (BrokenProgramStructure a b)    = (a, b)
    title (NoMainDefined {})              = "No entrypoint was defined"
    title (IdentifierConfliction {})      = "Identifier confliction"
    title (TypeContradiction {})          = "Type contradiction"
    title (InappropriateVarType {})       = "Inappropriate variable type"
    title (LoopFeatureOutsideLoop {})     = "Loop feature outside loop"
    title (UndefinedIdentifier {})        = "Undefined identifier"
    title (BrokenProgramStructure _ _)    = "Broken program structure"
    cause (NoMainDefined _ _)             = ""
    cause (IdentifierConfliction _ _ a)   =
        "A function or variable whose identifier is " ++ a ++ " is already defined."
    cause (TypeContradiction _ _ a b c d) =
        a ++ " has a type " ++ b ++ ", while the type of " ++ c ++ " is " ++ d ++ "."
    cause (InappropriateVarType _ _ a)    =
        a ++ " cannot be a type of variables."
    cause (LoopFeatureOutsideLoop _ _ a)  =
        "A loop feature " ++ a ++ " has used outside a loop syntax."
    cause (UndefinedIdentifier _ _ a)     =
        "Identifier " ++ a ++ " is not defined or inaccessible."
    cause (BrokenProgramStructure _ _)    = ""

semanticVerify :: String -> Syntax -> Either SemanticError ()
semanticVerify =
    mainDetection `andThen`
    conflictionCheck `andThen`
    variableTypeCheck `andThen`
    continueAndBreakCheck `andThen`
    identifierReferenceCheck
    where
    andThen f1 f2 a b = f1 a b >>= const (f2 a b)

mainDetection :: String -> Syntax -> Either SemanticError ()
mainDetection source (Program[Definitions defs]) =
    case filter (isTypeInt &&& isNameMain &&& isArgEmpty) defs of
        [] -> Left $ NoMainDefined source 0
        _  -> Right ()

    where
    isTypeInt (FunDefinition (_, Keyword "int") _ _ _) = True
    isTypeInt _ = False

    isNameMain (FunDefinition _ (_, Identifier "main") _ _) = True
    isNameMain _ = False

    -- TODO: command line arguments
    isArgEmpty (FunDefinition _ _ [] _) = True
    isArgEmpty _ = False

    (&&&) f1 f2 a = f1 a && f2 a

mainDetection source _ = Left $ BrokenProgramStructure source 0

conflictionCheck :: String -> Syntax -> Either SemanticError ()
conflictionCheck source (Program[Definitions defs]) = checkScope defs
    where
    checkScope programs = fst $ foldl
        (\a b ->
            case a of
                (Left _, _) -> a
                (Right (), ids) ->
                    case b of
                        (FunDefinition _ (n, Identifier name) _ _) | name `elem` ids ->
                            (Left $ IdentifierConfliction source n name, [])
                        
                        (VarDefinition _ (n, Identifier name) _) | name `elem` ids ->
                            (Left $ IdentifierConfliction source n name, [])

                        (FunDefinition _ (_, Identifier name) args body) ->
                            (checkScope (args ++ body), ids ++ [name])

                        (VarDefinition _ (_, Identifier name) _) ->
                            (Right (), ids ++ [name])

                        (While _ body) ->
                            (checkScope body, ids)

                        (If _ body1 body2) ->
                            (checkScope body1 >>= const (checkScope body2), ids)

                        (For _ _ _ body) ->
                            (checkScope body, ids)
                        
                        _ -> a

        )
        (Right (), [])
        programs

conflictionCheck source _ = Left $ BrokenProgramStructure source 0

variableTypeCheck :: String -> Syntax -> Either SemanticError ()
variableTypeCheck source (Program[Definitions defs]) = checkSequence defs
    where
    check :: Syntax -> Either SemanticError ()
    check (VarDefinition (n, Keyword "void") _ _) =
        Left $ InappropriateVarType source n "void"

    check (FunDefinition _ _ args body) = checkSequence $ args ++ body
    check (While _ body)                = checkSequence body
    check (If _ body1 body2)            = checkSequence $ body1 ++ body2
    check (For _ _ _ body)              = checkSequence body
    check _                             = Right ()

    checkSequence :: [Syntax] -> Either SemanticError ()
    checkSequence xs = mapM check xs >>= const (Right ())

variableTypeCheck source _ = Left $ BrokenProgramStructure source 0

continueAndBreakCheck :: String -> Syntax -> Either SemanticError ()
continueAndBreakCheck source (Program[Definitions defs]) = check defs False
    where
    check program insideLoop = foldl
        (\a b ->
            case a of
                (Left _) -> a
                (Right ()) ->
                    case b of
                        (Continue n) | not insideLoop ->
                            Left $ LoopFeatureOutsideLoop source n "continue"

                        (Break n) | not insideLoop ->
                            Left $ LoopFeatureOutsideLoop source n "break"

                        (FunDefinition _ _ args body) ->
                            check (args ++ body) insideLoop

                        (While _ body) ->
                            check body True

                        (If _ body1 body2) ->
                            check (body1 ++ body2) insideLoop

                        (For _ _ _ body) ->
                            check body True

                        _ -> a

        )
        (Right ())
        program

continueAndBreakCheck source _ = Left $ BrokenProgramStructure source 0

data AbstractDef = AbstractFun String Int
                        | AbstractVar String
                        deriving (Eq, Show)

identifierReferenceCheck :: String -> Syntax -> Either SemanticError ()
identifierReferenceCheck source (Program[Definitions defs]) = fst $ check [] defs
    where
    check:: [AbstractDef] -> [Syntax] -> (Either SemanticError (), [AbstractDef])
    check ids = foldl
        (\a b ->
            case a of
                (Left _, _) -> a
                (Right (), ids') ->
                    case b of
                        (FunDefinition _ (_, Identifier x) args body) ->
                            let abstractDef = AbstractFun x (length args) in
                            ( fst $ check (ids' ++ [abstractDef]) (args ++ body)
                            , ids' ++ [abstractDef]
                            )

                        (VarDefinition _ (_, Identifier x) (Just expr)) ->
                            ( checkExpr ids' [expr]
                            , ids' ++ [AbstractVar x]
                            )

                        (VarDefinition _ (_, Identifier x) Nothing) ->
                            (Right (), ids' ++ [AbstractVar x])

                        (VarReassign (_, Identifier x) expr)
                            | AbstractVar x `elem` ids' ->
                                (checkExpr ids' [expr], ids')

                        (FunctionCallSyntax (_, Identifier x) expr)
                            | AbstractFun x (length expr) `elem` ids' ->
                                (checkExpr ids' expr, ids')

                        (While expr body) ->
                            ( checkExpr ids' [expr] >>=
                                const (fst $ check ids' body)
                            , ids'
                            )

                        (If expr body1 body2) ->
                            ( checkExpr ids' [expr] >>=
                                const (fst $ check ids' body1) >>=
                                    const (fst $ check ids' body2)
                            , ids'
                            )

                        (For maybeBody1 maybeExpr maybeBody2 body3) ->
                            let body1 = case maybeBody1 of
                                            Just s -> [s]
                                            Nothing -> []
                                body2 = case maybeBody2 of
                                            Just s -> [s]
                                            Nothing -> []
                                expr  = case maybeExpr of
                                            Just e -> [e]
                                            Nothing -> []
                            in
                            ( fst
                                ( case check ids' body1 of
                                    (Left e, _) -> (Left e, [])
                                    (Right (), ids'') ->
                                        case checkExpr ids'' expr of
                                            (Left e') -> (Left e', [])
                                            (Right ()) ->
                                                case check ids'' body2 of
                                                    (Left e'', _) -> (Left e'', [])
                                                    (Right (), ids''') ->
                                                        check ids''' body3
                                )
                            , ids'
                            )

                        (Return expr) ->
                            (checkExpr ids' [expr], ids')

                        (VarReassign (n, Identifier x) _) ->
                            (Left $ UndefinedIdentifier source n x, [])

                        (FunctionCallSyntax (n, Identifier x) _) ->
                            (Left $ UndefinedIdentifier source n x, [])
                        
                        (VarDefinition {}) ->
                            (Left $ BrokenProgramStructure source 0, [])

                        (FunDefinition {}) ->
                            (Left $ BrokenProgramStructure source 0, [])

                        (VarReassign {}) ->
                            (Left $ BrokenProgramStructure source 0, [])

                        (FunctionCallSyntax {}) ->
                            (Left $ BrokenProgramStructure source 0, [])

                        (Program _) ->
                            (Left $ BrokenProgramStructure source 0, [])

                        (Definitions _) ->
                            (Left $ BrokenProgramStructure source 0, [])

                        (Continue _) ->
                            (Right (), ids')
                        
                        (Break _) ->
                            (Right (), ids')
        )
        (Right (), ids)

    checkExpr :: [AbstractDef] -> [Expression] -> Either SemanticError ()
    checkExpr ids = foldl
        (\a b ->
            case a of
                (Left _) -> a
                (Right ()) ->
                    case b of
                        (VarReference (_, Identifier x))
                            | AbstractVar x `elem` ids ->
                                Right()

                        (FunctionCall (_, Identifier x) args)
                            | AbstractFun x (length args) `elem` ids ->
                                checkExpr ids args

                        (Addition expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (Subtraction expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (Multiplication expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (Division expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (Equals expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (NotEquals expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (MoreThan expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (LessThan expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (MoreThanOrEq expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (LessThanOrEq expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (Opposition expr) ->
                            checkExpr ids [expr]

                        (Negative expr) ->
                            checkExpr ids [expr]

                        (And expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (Or expr1 expr2) ->
                            checkExpr ids [expr1, expr2]

                        (VarReference (n, Identifier x)) ->
                            Left $ UndefinedIdentifier source n x

                        (FunctionCall (n, Identifier x) _) ->
                            Left $ UndefinedIdentifier source n x

                        (VarReference _) ->
                            Left $ BrokenProgramStructure source 0

                        (FunctionCall _ _) ->
                            Left $ BrokenProgramStructure source 0

                        (NumReference _) ->
                            Right ()

                        Void ->
                            Right ()
        )
        (Right ())

identifierReferenceCheck source _ = Left $ BrokenProgramStructure source 0
