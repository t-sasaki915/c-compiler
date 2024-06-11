module SyntaxAnalyser
  ( SyntaxAnalyserError(..)
  , Syntax(..)
  , SyntaxTree(..)
  , syntaxAnalyse
  ) where

import ExpressionAnalyser (Expression(..))
import SourceFileAnalyser (sourceLoc)
import Tokeniser (Token(..))

data SyntaxAnalyserError = UnexpectedToken String Int Token String
                         | UnexpectedEOF String Int
                         deriving Eq

instance Show SyntaxAnalyserError where
    show (UnexpectedToken src ind t e) =
        "(" ++ sourceLoc src ind ++ ") Expected " ++ e ++ " but " ++ show t ++ " found."
    show (UnexpectedEOF src ind) =
        "(" ++ sourceLoc src ind ++ ") Unexpected end of file."

data Syntax = Program
            -- FunctionDef Type Identifier [ArgumentVariableDef] [Content]
            | FunctionDef (Int, Token) (Int, Token) [Syntax] [Syntax]
            -- VariableDef Type Identifier (Maybe DefaultValue)
            | VariableDef (Int, Token) (Int, Token) (Maybe Expression)
            -- Return Value
            | Return Expression
            deriving (Show, Eq)

data SyntaxTree = SyntaxTree
    { _rootSyntax :: Syntax
    , _subSyntax :: [SyntaxTree]
    }
    deriving Show

syntaxAnalyse :: String -> [(Int, Token)] -> Either SyntaxAnalyserError SyntaxTree
syntaxAnalyse source tokens = Right $ SyntaxTree Program []
