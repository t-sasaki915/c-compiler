module SyntaxAnalyserSpecDomain
    ( Result
    , source1
    , expect1
    ) where

import SyntaxAnalyser
import Tokeniser (Token(..))

type Result = Either SyntaxAnalyserError SyntaxTree

source1 :: String
source1 = unlines
    [ "int a;"
    , "int b;"
    , "void c;"
    ]

expect1 :: Result
expect1 = Right $
    SyntaxTree Program
        [ SyntaxTree (VarDefinition (2, Keyword "int") (4, Identifier "a") Nothing) []
        , SyntaxTree (VarDefinition (9, Keyword "int") (11, Identifier "b") Nothing) []
        , SyntaxTree (VarDefinition (17, Keyword "void") (19, Identifier "c") Nothing) []
        ]
