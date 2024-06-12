module ExpressionAnalyserSpecDomain
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
    , source19
    , expect19
    , source20
    , expect20
    ) where
    
import ExpressionAnalyser
import Tokeniser (Token(..))

type Result = Either ExpressionAnalyserError (Int, Expression)

source1 :: String
source1 = "a;"

expect1 :: Result
expect1 = Right
    (1, VarReference (0, Identifier "a"))

source2 :: String
source2 = "1;"

expect2 :: Result
expect2 = Right
    (1, NumReference (0, Number "1"))

source3 :: String
source3 = "(a);"

expect3 :: Result
expect3 = Right
    (3, VarReference (1, Identifier "a"))

source4 :: String
source4 = "(((1)));"

expect4 :: Result
expect4 = Right
    (7, NumReference (3, Number "1"))

source5 :: String
source5 = "f();"

expect5 :: Result
expect5 = Right
    (3, FunctionCall (0, Identifier "f") [])

source6 :: String
source6 = "f(a);"

expect6 :: Result
expect6 = Right
    ( 4
    , FunctionCall (0, Identifier "f")
        [ VarReference (2, Identifier "a")
        ]
    )

source7 :: String
source7 = "f(a, b);"

expect7 :: Result
expect7 = Right
    ( 6
    , FunctionCall (0, Identifier "f")
        [ VarReference (2, Identifier "a")
        , VarReference (5, Identifier "b")
        ]
    )

source8 :: String
source8 = "f(g(h(0)), g(b), c);"

expect8 :: Result
expect8 = Right
    ( 17
    , FunctionCall (0, Identifier "f")
        [ FunctionCall (2, Identifier "g")
            [ FunctionCall (4, Identifier "h")
                [ NumReference (6, Number "0")
                ]
            ]
        , FunctionCall (11, Identifier "g")
            [ VarReference (13, Identifier "b")
            ]
        , VarReference (17, Identifier "c")
        ]
    )

source9 :: String
source9 = "a + b;"

expect9 :: Result
expect9 = Right
    ( 3
    , Addition
        (VarReference (0, Identifier "a"))
        (VarReference (4, Identifier "b"))
    )

source10 :: String
source10 = "a + b + c;"

expect10 :: Result
expect10 = Right
    ( 5
    , Addition
        ( Addition
            (VarReference (0, Identifier "a"))
            (VarReference (4, Identifier "b"))
        )
        (VarReference (8, Identifier "c"))
    )

source11 :: String
source11 = "(a + b) + c;"

expect11 :: Result
expect11 = Right
    ( 7
    , Addition
        ( Addition
            (VarReference (1, Identifier "a"))
            (VarReference (5, Identifier "b"))
        )
        (VarReference (10, Identifier "c"))
    )

source12 :: String
source12 = "a + (b + c);"

expect12 :: Result
expect12 = Right
    ( 7
    , Addition
        (VarReference (0, Identifier "a"))
        ( Addition
            (VarReference (5, Identifier "b"))
            (VarReference (9, Identifier "c"))
        )
    )

source13 :: String
source13 = "A * sin(2 * getPi() * f * t) + 1;"

expect13 :: Result
expect13 = Right
    ( 16
    , Addition
        ( Multiplication
            (VarReference (0, Identifier "A"))
            ( FunctionCall (6, Identifier "sin")
                [ Multiplication
                    ( Multiplication
                        ( Multiplication
                            (NumReference (8, Number "2"))
                            (FunctionCall (16, Identifier "getPi") [])
                        )
                        (VarReference (22, Identifier "f"))
                    )
                    (VarReference (26, Identifier "t"))
                ]
            )
        )
        (NumReference (31, Number "1"))
    )

source14 :: String
source14 = "a + b"

expect14 :: Result
expect14 = Left $
    UnexpectedEOF source14 4

source15 :: String
source15 = "a + );"

expect15 :: Result
expect15 = Left $
    UnexpectedToken
        source15
        4
        CloseParentheses
        "'(', Identifier or Number"

source16 :: String
source16 = "!a;"

expect16 :: Result
expect16 = Right
    ( 2
    , Opposition (VarReference (1, Identifier "a"))
    )

source17 :: String
source17 = "!(!a);"

expect17 :: Result
expect17 = Right
    ( 5
    , Opposition
        ( Opposition (VarReference (3, Identifier "a"))
        )
    )

source18 :: String
source18 = "!(a >= b);"

expect18 :: Result
expect18 = Right
    ( 6
    , Opposition
        ( MoreThanOrEq
            (VarReference (2, Identifier "a"))
            (VarReference (7, Identifier "b"))
        )
    )

source19 :: String
source19 = "!isExist(a + b);"

expect19 :: Result
expect19 = Right
    ( 7
    , Opposition
        ( FunctionCall (7, Identifier "isExist")
            [ Addition
                (VarReference (9, Identifier "a"))
                (VarReference (13, Identifier "b"))
            ]
        )
    )

source20 :: String
source20 = "f(!(a > b), !c, d <= e);"

expect20 :: Result
expect20 = Right
    ( 16
    , FunctionCall (0, Identifier "f")
        [ Opposition
            ( MoreThan
                (VarReference (4, Identifier "a"))
                (VarReference (8, Identifier "b"))
            )
        , Opposition (VarReference (13, Identifier "c"))
        , LessThanOrEq
            (VarReference (16, Identifier "d"))
            (VarReference (21, Identifier "e"))
        ]
    )
