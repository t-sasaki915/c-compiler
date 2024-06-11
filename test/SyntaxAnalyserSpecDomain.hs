module SyntaxAnalyserSpecDomain
    ( Result
    , source1
    , expect1
    , source2
    , expect2
    ) where

import SyntaxAnalyser
import Tokeniser (Token(..))
import ExpressionAnalyser (Expression(..))

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

source2 :: String
source2 = unlines
    [ "int a;"
    , "int b;"
    , "int c = a + b;"
    ]

expect2 :: Result
expect2 = Right $
    SyntaxTree Program
        [ SyntaxTree (VarDefinition (2, Keyword "int") (4, Identifier "a") Nothing) []
        , SyntaxTree (VarDefinition (9, Keyword "int") (11, Identifier "b") Nothing) []
        , SyntaxTree
            ( VarDefinition
                (16, Keyword "int")
                (18, Identifier "c")
                ( Just
                    ( Addition
                        (VarReference (22, Identifier "a"))
                        (VarReference (26, Identifier "b"))
                    )
                )
            ) []
        ]
