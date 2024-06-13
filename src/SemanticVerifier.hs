module SemanticVerifier
    ( SemanticError(..)
    , semanticVerify
    , mainDetection
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
semanticVerify = mainDetection

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
