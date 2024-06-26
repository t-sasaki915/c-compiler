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
    , source6
    , expect6
    , source7
    , expect7
    , source8
    , expect8
    , source9
    , expect9
    , source10
    , expect10
    , source11
    , expect11
    , source12
    , expect12
    , source13
    , expect13
    , source14
    , expect14
    , source15
    , expect15
    , source16
    , expect16
    , source17
    , expect17
    , source18
    , expect18
    ) where

import SyntaxAnalyser
import Tokeniser (Token(..))
import ExpressionAnalyser (Expression(..))

type Result = Either SyntaxAnalyserError Syntax

source1 :: String
source1 = unlines
    [ "int a;"
    , "int b;"
    , "void c;"
    ]

expect1 :: Result
expect1 = Right $
    Program
        [ Definitions
            [ VarDefinition (2, Keyword "int") (4, Identifier "a") Nothing
            , VarDefinition (9, Keyword "int") (11, Identifier "b") Nothing
            , VarDefinition (17, Keyword "void") (19, Identifier "c") Nothing
            ]
        ]

source2 :: String
source2 = unlines
    [ "int a;"
    , "int b;"
    , "int c = a + b;"
    ]

expect2 :: Result
expect2 = Right $
    Program
        [ Definitions
            [ VarDefinition (2, Keyword "int") (4, Identifier "a") Nothing
            , VarDefinition (9, Keyword "int") (11, Identifier "b") Nothing
            , VarDefinition
                (16, Keyword "int")
                (18, Identifier "c")
                ( Just
                    ( Addition
                        (VarReference (22, Identifier "a"))
                        (VarReference (26, Identifier "b"))
                    )
                )
            ]
        ]

source3 :: String
source3 = unlines
    [ "int main(void) {}"
    , "void doNothing() {}"
    ]

expect3 :: Result
expect3 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (2, Keyword "int")
                (7, Identifier "main")
                []
                []
            , FunDefinition
                (21, Keyword "void")
                (31, Identifier "doNothing")
                []
                []
            ]
        ]

source4 :: String
source4 = unlines
    [ "int isPrime(int x) {}"
    , "int add(int a, int b) {}"
    , "int largest3(int a, int b, int c) {}"
    ]

expect4 :: Result
expect4 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (2, Keyword "int")
                (10, Identifier "isPrime")
                [ VarDefinition (14, Keyword "int") (16, Identifier "x") Nothing
                ]
                []
            , FunDefinition
                (24, Keyword "int")
                (28, Identifier "add")
                [ VarDefinition (32, Keyword "int") (34, Identifier "a") Nothing
                , VarDefinition (39, Keyword "int") (41, Identifier "b") Nothing
                ]
                []
            , FunDefinition
                (49, Keyword "int")
                (58, Identifier "largest3")
                [ VarDefinition (62, Keyword "int") (64, Identifier "a") Nothing
                , VarDefinition (69, Keyword "int") (71, Identifier "b") Nothing
                , VarDefinition (76, Keyword "int") (78, Identifier "c") Nothing
                ]
                []
            ]
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
    Program
        [ Definitions
            [ FunDefinition
                (2, Keyword "int")
                (7, Identifier "main")
                []
                [ Return (NumReference (28, Number "0"))
                ]
            , FunDefinition
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
            ]
        ]

source6 :: String
source6 = unlines
    [ "void doNothing()"
    , "{"
    , "    return;"
    , "}"
    ]

expect6 :: Result
expect6 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (13, Identifier "doNothing")
                []
                [ Return Void
                ]
            ]
        ]

source7 :: String
source7 = unlines
    [ "int aaa(void)"
    , "{"
    , "    int a;"
    , "    int b;"
    , "    int c = a + b;"
    , "    return c;"
    , "}"
    ]

expect7 :: Result
expect7 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (2, Keyword "int")
                (6, Identifier "aaa")
                []
                [ VarDefinition (22, Keyword "int") (24, Identifier "a") Nothing
                , VarDefinition (33, Keyword "int") (35, Identifier "b") Nothing
                , VarDefinition (44, Keyword "int") (46, Identifier "c")
                    ( Just
                        ( Addition
                            (VarReference (50, Identifier "a"))
                            (VarReference (54, Identifier "b"))
                        )
                    )
                , Return (VarReference (68, Identifier "c"))
                ]
            ]
        ]

source8 :: String
source8 = unlines
    [ "void aaa()"
    , "{"
    , "    int a;"
    , "    a = 0;"
    , "    a = a + 1;"
    , "}"
    ]

expect8 :: Result
expect8 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ VarDefinition (19, Keyword "int") (21, Identifier "a") Nothing
                , VarReassign (28, Identifier "a") (NumReference (32, Number "0"))
                , VarReassign (39, Identifier "a")
                    ( Addition
                        (VarReference (43, Identifier "a"))
                        (NumReference (47, Number "1"))
                    )
                ]
            ]
        ]

source9 :: String
source9 = unlines
    [ "void aaa()"
    , "{"
    , "    bbb();"
    , "    ccc(1);"
    , "    ddd(a + b, aaa());"
    , "}"
    ]

expect9 :: Result
expect9 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ FunctionCallSyntax (19, Identifier "bbb") []
                , FunctionCallSyntax (30, Identifier "ccc")
                    [ NumReference (32, Number "1")
                    ]
                , FunctionCallSyntax (42, Identifier "ddd")
                    [ Addition 
                        (VarReference (44, Identifier "a"))
                        (VarReference (48, Identifier "b"))
                    , FunctionCall (53, Identifier "aaa") []
                    ]
                ]
            ]
        ]

source10 :: String
source10 = unlines
    [ "void aaa()"
    , "{"
    , "    while (a > b)"
    , "    {"
    , "        bbb();"
    , "    }"
    , "}"
    ]

expect10 :: Result
expect10 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ While
                    ( MoreThan
                        (VarReference (24, Identifier "a"))
                        (VarReference (28, Identifier "b"))
                    )
                    [ FunctionCallSyntax (47, Identifier "bbb") []
                    ]
                ]
            ]
        ]

source11 :: String
source11 = unlines
    [ "void aaa()"
    , "{"
    , "    while (a)"
    , "        bbb();"
    , "    while (b)"
    , "        while (c)"
    , "        {"
    , "            ccc();"
    , "        }"
    , "}"
    ]

expect11 :: Result
expect11 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ While (VarReference (24, Identifier "a"))
                    [ FunctionCallSyntax (37, Identifier "bbb") []
                    ]
                , While (VarReference (53, Identifier "b"))
                    [ While (VarReference (71, Identifier "c"))
                        [ FunctionCallSyntax (98, Identifier "ccc") []
                        ]
                    ]
                ]
            ]
        ]

source12 :: String
source12 = unlines
    [ "void aaa()"
    , "{"
    , "    if (a)"
    , "    {"
    , "        aaa();"
    , "    }"
    , "    if (b)"
    , "    {"
    , "        bbb();"
    , "    }"
    , "    else"
    , "    {"
    , "        ccc();"
    , "    }"
    , "}"
    ]

expect12 :: Result
expect12 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ If (VarReference (21, Identifier "a"))
                    [ FunctionCallSyntax (40, Identifier "aaa") []
                    ]
                    []
                , If (VarReference (59, Identifier "b"))
                    [ FunctionCallSyntax (78, Identifier "bbb") []
                    ]
                    [ FunctionCallSyntax (114, Identifier "ccc") []
                    ]
                ]
            ]
        ]

source13 :: String
source13 = unlines
    [ "void aaa()"
    , "{"
    , "    if (a)"
    , "        aaa();"
    , "    bbb();"
    , "    if (b)"
    , "        ccc();"
    , "    else if (c)"
    , "        ddd();"
    , "}"
    ]

expect13 :: Result
expect13 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ If (VarReference (21, Identifier "a"))
                    [ FunctionCallSyntax (34, Identifier "aaa") []
                    ]
                    []
                , FunctionCallSyntax (45, Identifier "bbb") []
                , If (VarReference (58, Identifier "b"))
                    [ FunctionCallSyntax (71, Identifier "ccc") []
                    ]
                    [ If (VarReference (89, Identifier "c"))
                        [ FunctionCallSyntax (102, Identifier "ddd") []
                        ]
                        []
                    ]
                ]
            ]
        ]

source14 :: String
source14 = unlines
    [ "void aaa()"
    , "{"
    , "    a += 1;"
    , "    b -= c();"
    , "}"
    ]

expect14 :: Result
expect14 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ VarReassign (17, Identifier "a")
                    ( Addition
                        (VarReference (17, Identifier "a"))
                        (NumReference (22, Number "1"))
                    )
                , VarReassign (29, Identifier "b")
                    ( Subtraction
                        (VarReference (29, Identifier "b"))
                        (FunctionCall (34, Identifier "c") [])
                    )
                ]
            ]
        ]

source15 :: String
source15 = unlines
    [ "void aaa()"
    , "{"
    , "    a ++;"
    , "    b--;"
    , "}"
    ]

expect15 :: Result
expect15 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ VarReassign (17, Identifier "a")
                    ( Addition
                        (VarReference (17, Identifier "a"))
                        (NumReference (20, Number "1"))
                    )
                , VarReassign (27, Identifier "b")
                    ( Subtraction
                        (VarReference (27, Identifier "b"))
                        (NumReference (29, Number "1"))
                    )
                ]
            ]
        ]

source16 :: String
source16 = unlines
    [ "void aaa()"
    , "{"
    , "    ++a;"
    , "    -- b;"
    , "}"
    ]

expect16 :: Result
expect16 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ VarReassign (19, Identifier "a")
                    ( Addition
                        (VarReference (19, Identifier "a"))
                        (NumReference (18, Number "1"))
                    )
                , VarReassign (29, Identifier "b")
                    ( Subtraction
                        (VarReference (29, Identifier "b"))
                        (NumReference (27, Number "1"))
                    )
                ]
            ]
        ]

source17 :: String
source17 = unlines
    [ "void aaa()"
    , "{"
    , "    for (int a = 0; a < 100; a++)"
    , "    {"
    , "        bbb();"
    , "    }"
    , "}"
    ]

expect17 :: Result
expect17 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ For
                    ( Just
                        ( VarDefinition
                            (24, Keyword "int")
                            (26, Identifier "a")
                            (Just (NumReference (30, Number "0")))
                        )
                    )
                    ( Just
                        ( LessThan
                            (VarReference (33, Identifier "a"))
                            (NumReference (39, Number "100"))
                        )
                    )
                    ( Just
                        ( VarReassign
                            (42, Identifier "a")
                            ( Addition
                                (VarReference (42, Identifier "a"))
                                (NumReference (44, Number "1"))
                            )
                        )
                    )
                    [ FunctionCallSyntax (63, Identifier "bbb") []
                    ]
                ]
            ]
        ]

source18 :: String
source18 = unlines
    [ "void aaa()"
    , "{"
    , "    for (;;)"
    , "        for (;;)"
    , "            bbb();"
    , "    ccc();"
    , "}"
    ]

expect18 :: Result
expect18 = Right $
    Program
        [ Definitions
            [ FunDefinition
                (3, Keyword "void")
                (7, Identifier "aaa")
                []
                [ For Nothing Nothing Nothing
                    [ For Nothing Nothing Nothing
                        [ FunctionCallSyntax (57, Identifier "bbb") []
                        ]
                    ]
                , FunctionCallSyntax (68, Identifier "ccc") []
                ]
            ]
        ]
