{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module SemanticVerifier
    ( SemanticError(..)
    , semanticVerify
    , mainDetection
    , conflictionCheck
    ) where

import ErrorHandling
import SyntaxAnalyser (Syntax(..))
import Tokeniser (Token(..))

data SemanticError = NoMainDefined String Int
                   | IdentifierConfliction String Int String
                   | TypeContradiction String Int String String String String
                   | BrokenProgramStructure String Int
                   deriving (Show, Eq)

instance TrackableError SemanticError where
    place (NoMainDefined a b)             = (a, b)
    place (IdentifierConfliction a b _)   = (a, b)
    place (TypeContradiction a b _ _ _ _) = (a, b)
    place (BrokenProgramStructure a b)    = (a, b)
    title (NoMainDefined {})              = "No entrypoint was defined"
    title (IdentifierConfliction {})      = "Identifier confliction"
    title (TypeContradiction {})          = "Type contradiction"
    title (BrokenProgramStructure _ _)    = "Broken program structure"
    cause (NoMainDefined _ _)             = ""
    cause (IdentifierConfliction _ _ a)   =
        "A function or variable whose identifier is " ++ a ++ " is already defined."
    cause (TypeContradiction _ _ a b c d) =
        a ++ " has a type " ++ b ++ ", while the type of " ++ c ++ " is " ++ d ++ "."
    cause (BrokenProgramStructure _ _)    = ""

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) f1 f2 a = f1 a && f2 a

semanticVerify :: String -> Syntax -> Either SemanticError ()
semanticVerify =
    mainDetection `andThen`
    conflictionCheck
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

mainDetection source _ = Left $ BrokenProgramStructure source 0

conflictionCheck :: String -> Syntax -> Either SemanticError ()
conflictionCheck source (Program[Definitions defs]) =
    globally [] 0 >>= const (functionScope 0)
    where
    globally :: [String] -> Int -> Either SemanticError ()
    globally _ index | index >= length defs = Right ()
    globally detected index =
        case defs !! index of
            (FunDefinition _ (n, Identifier name) _ _) | name `elem` detected ->
                Left $ IdentifierConfliction source n name

            (FunDefinition _ (_, Identifier name) _ _) ->
                globally (detected ++ [name]) (index + 1)

            (VarDefinition _ (n, Identifier name) _) | name `elem` detected ->
                Left $ IdentifierConfliction source n name

            (VarDefinition _ (_, Identifier name) _) ->
                globally (detected ++ [name]) (index + 1)

            _ ->
                Left $ BrokenProgramStructure source 0

    functionScope :: Int -> Either SemanticError ()
    functionScope index | index >= length defs = Right ()
    functionScope index =
        case defs !! index of
            (FunDefinition _ _ args body) ->
                (check (args ++ body) [] 0) >>=
                    const (functionScope (index + 1))

            _ ->
                functionScope (index + 1)
        where
        check :: [Syntax] -> [String] -> Int -> Either SemanticError ()
        check defs2 _ index2 | index2 >= length defs2 = Right ()
        check defs2 detected index2 =
            case defs2 !! index2 of
                (VarDefinition _ (n, Identifier name) _) | name `elem` detected ->
                    Left $ IdentifierConfliction source n name

                (VarDefinition _ (_, Identifier name) _) ->
                    check defs2 (detected ++ [name]) (index2 + 1)

                _ ->
                    check defs2 detected (index2 + 1)

conflictionCheck source _ = Left $ BrokenProgramStructure source 0
