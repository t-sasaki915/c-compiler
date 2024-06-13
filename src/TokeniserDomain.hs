module TokeniserDomain
    ( acceptableWhitespaces
    , acceptableReturns
    , acceptableSymbols
    , acceptableAlphabets
    , acceptableNumbers
    , acceptableKeywords
    , typeKeywords
    ) where

acceptableWhitespaces :: [Char]
acceptableWhitespaces =
    [ ' ', '\t' ]

acceptableReturns :: [Char]
acceptableReturns =
    [ '\n' ]

acceptableSymbols :: [Char]
acceptableSymbols =
    [ '+', '-', '*', '/', '=', '<', '>', '!', '&', '|'
    ]

acceptableAlphabets :: [Char]
acceptableAlphabets =
    [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm'
    , 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
    , 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M'
    , 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
    ]

acceptableNumbers :: [Char]
acceptableNumbers =
    [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
    ]

typeKeywords :: [String]
typeKeywords =
    [ "int", "void"
    ]

acceptableKeywords :: [String]
acceptableKeywords = typeKeywords ++
    [ "return", "while", "if", "else", "continue", "break", "for"
    ]
