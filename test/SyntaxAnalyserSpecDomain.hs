module SyntaxAnalyserSpecDomain
    ( Result
    , source1
    , expect1
    , source2
    , expect2
    , source3
    , expect3
    , source4
    , expect4
    , source5
    , expect5
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

source3 :: String
source3 = unlines
    [ "int main(void) {}"
    , "void doNothing() {}"
    ]

expect3 :: Result
expect3 = Right $
    SyntaxTree Program
        [ SyntaxTree
            ( FunDefinition
                (2, Keyword "int")
                (7, Identifier "main")
                []
                []
            ) []
        , SyntaxTree
            ( FunDefinition
                (21, Keyword "void")
                (31, Identifier "doNothing")
                []
                []
            ) []
        ]

source4 :: String
source4 = unlines
    [ "int isPrime(int x) {}"
    , "int add(int a, int b) {}"
    , "int largest3(int a, int b, int c) {}"
    ]

expect4 :: Result
expect4 = Right $
    SyntaxTree Program
        [ SyntaxTree
            ( FunDefinition
                (2, Keyword "int")
                (10, Identifier "isPrime")
                [ VarDefinition (14, Keyword "int") (16, Identifier "x") Nothing
                ]
                []
            ) []
        , SyntaxTree
            ( FunDefinition
                (24, Keyword "int")
                (28, Identifier "add")
                [ VarDefinition (32, Keyword "int") (34, Identifier "a") Nothing
                , VarDefinition (39, Keyword "int") (41, Identifier "b") Nothing
                ]
                []
            ) []
        , SyntaxTree
            ( FunDefinition
                (49, Keyword "int")
                (58, Identifier "largest3")
                [ VarDefinition (62, Keyword "int") (64, Identifier "a") Nothing
                , VarDefinition (69, Keyword "int") (71, Identifier "b") Nothing
                , VarDefinition (76, Keyword "int") (78, Identifier "c") Nothing
                ]
                []
            ) []
        ]

source5 :: String
source5 = unlines
    [ "int main(void)"
    , "{"
    , "    return 0;"
    , "}"
    , "int add(int a, int b)"
    , "{"
    , "    return a + b;"
    , "}"
    ]

expect5 :: Result
expect5 = Right $
    SyntaxTree Program
        [ SyntaxTree
            ( FunDefinition
                (2, Keyword "int")
                (7, Identifier "main")
                []
                [ Return (NumReference (28, Number "0"))
                ]
            ) []
        , SyntaxTree
            ( FunDefinition
                (35, Keyword "int")
                (39, Identifier "add")
                [ VarDefinition (43, Keyword "int") (45, Identifier "a") Nothing
                , VarDefinition (50, Keyword "int") (52, Identifier "b") Nothing
                ]
                [ Return 
                    ( Addition
                        (VarReference (68, Identifier "a"))
                        (VarReference (72, Identifier "b"))
                    )
                ]
            ) []
        ]
