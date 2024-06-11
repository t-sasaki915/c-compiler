module TokeniserSpecDomain
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
    ) where

import Tokeniser

type Result = Either TokeniserError [(Int, Token)]

source1 :: String
source1 = unlines
    [ "int main(void)"
    , "{"
    , "    return 0;"
    , "}"
    ]

expect1 :: Result
expect1 = Right
    [ (2, Keyword "int")
    , (7, Identifier "main")
    , (8, OpenParentheses)
    , (12, Keyword "void")
    , (13, CloseParentheses)
    , (15, OpenBrace)
    , (26, Keyword "return")
    , (28, Number "0")
    , (29, Semicolon)
    , (31, CloseBrace)
    ]

source2 :: String
source2 = unlines
    [ "int add(int a, int b)"
    , "{"
    , "    return a + b;"
    , "}"
    ]

expect2 :: Result
expect2 = Right
    [ (2, Keyword "int")
    , (6, Identifier "add")
    , (7, OpenParentheses)
    , (10, Keyword "int")
    , (12, Identifier "a")
    , (13, Comma)
    , (17, Keyword "int")
    , (19, Identifier "b")
    , (20, CloseParentheses)
    , (22, OpenBrace)
    , (33, Keyword "return")
    , (35, Identifier "a")
    , (37, Symbol '+')
    , (39, Identifier "b")
    , (40, Semicolon)
    , (42, CloseBrace)
    ]

source3 :: String
source3 = unlines
    [ "// AAA"
    , "int main(void)"
    , "{   // Returns 0."
    , "    return 0; // 1"
    , "} // End of main."
    ]

expect3 :: Result
expect3 = Right
    [ (9, Keyword "int")
    , (14, Identifier "main")
    , (15, OpenParentheses)
    , (19, Keyword "void")
    , (20, CloseParentheses)
    , (22, OpenBrace)
    , (49, Keyword "return")
    , (51, Number "0")
    , (52, Semicolon)
    , (59, CloseBrace)
    ]

source4 :: String
source4 = unlines
    [ "/*"
    , "AAA"
    , "BBB"
    , "*/"
    , "int ma/*aaa*/in(vo/*oooo*/id)"
    , "{"
    , "    /* Returns 0.*/"
    , "    return 0 /*CCC*/;"
    , "} /* End of main."
    ]

expect4 :: Result
expect4 = Right
    [ (16, Keyword "int")
    , (28, Identifier "main")
    , (29, OpenParentheses)
    , (41, Keyword "void")
    , (42, CloseParentheses)
    , (44, OpenBrace)
    , (75, Keyword "return")
    , (77, Number "0")
    , (86, Semicolon)
    , (88, CloseBrace)
    ]

source5 :: String
source5 = unlines
    [ "int @main(void) {}"
    ]

expect5 :: Result
expect5 = Left $
    UnexpectedCharacter source5 4 '@'

source6 :: String
source6 = "a == b"

expect6 :: Result
expect6 = Right
    [ (0, Identifier "a")
    , (3, Equality)
    , (5, Identifier "b")
    ]

source7 :: String
source7 = "a ! = b"

expect7 :: Result
expect7 = Right
    [ (0, Identifier "a")
    , (2, Symbol '!')
    , (4, Symbol '=')
    , (6, Identifier "b")
    ]
