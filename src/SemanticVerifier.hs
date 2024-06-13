module SemanticVerifier
    ( SemanticError(..)
    , semanticVerify
    , mainDetection
    , conflictionCheck
    , variableTypeCheck
    ) where

import ErrorHandling
import SyntaxAnalyser (Syntax(..))
import Tokeniser (Token(..))

data SemanticError = NoMainDefined String Int
                   | IdentifierConfliction String Int String
                   | TypeContradiction String Int String String String String
                   | InappropriateVarType String Int String
                   | BrokenProgramStructure String Int
                   deriving (Show, Eq)

instance TrackableError SemanticError where
    place (NoMainDefined a b)             = (a, b)
    place (IdentifierConfliction a b _)   = (a, b)
    place (TypeContradiction a b _ _ _ _) = (a, b)
    place (InappropriateVarType a b _)    = (a, b)
    place (BrokenProgramStructure a b)    = (a, b)
    title (NoMainDefined {})              = "No entrypoint was defined"
    title (IdentifierConfliction {})      = "Identifier confliction"
    title (TypeContradiction {})          = "Type contradiction"
    title (InappropriateVarType {})       = "Inappropriate variable type"
    title (BrokenProgramStructure _ _)    = "Broken program structure"
    cause (NoMainDefined _ _)             = ""
    cause (IdentifierConfliction _ _ a)   =
        "A function or variable whose identifier is " ++ a ++ " is already defined."
    cause (TypeContradiction _ _ a b c d) =
        a ++ " has a type " ++ b ++ ", while the type of " ++ c ++ " is " ++ d ++ "."
    cause (InappropriateVarType _ _ a)    =
        a ++ " cannot be a type of variables."
    cause (BrokenProgramStructure _ _)    = ""

semanticVerify :: String -> Syntax -> Either SemanticError ()
semanticVerify =
    mainDetection `andThen`
    conflictionCheck `andThen`
    variableTypeCheck
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
