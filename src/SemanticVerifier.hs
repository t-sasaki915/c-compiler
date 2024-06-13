module SemanticVerifier (SemanticError(..), semanticVerify) where

import ErrorHandling
import SyntaxAnalyser (Syntax(..))

data SemanticError = NoMainDefined String Int
                   | IdentifierConfliction String Int String
                   | TypeContradiction String Int String String String String

instance TrackableError SemanticError where
    place (NoMainDefined a b)             = (a, b)
    place (IdentifierConfliction a b _)   = (a, b)
    place (TypeContradiction a b _ _ _ _) = (a, b)
    title (NoMainDefined {})              = "No 'main' defined"
    title (IdentifierConfliction {})      = "Identifier confliction"
    title (TypeContradiction {})          = "Type contradiction"
    cause (NoMainDefined {})              = ""
    cause (IdentifierConfliction _ _ a)   =
        "A function or variable whose identifier is " ++ a ++ " is already defined."
    cause (TypeContradiction _ _ a b c d) =
        a ++ " has a type " ++ b ++ " while the type of " ++ c ++ " is " ++ d ++ "."

semanticVerify :: String -> Syntax -> Either SemanticError ()
semanticVerify source program = Right ()
