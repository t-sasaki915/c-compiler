module SemanticVerifierSpecDomain
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

import SemanticVerifier

type Result = Either SemanticError ()

source1 :: String
source1 = "int main(void) {}"

expect1 :: Result
expect1 = Right ()

source2 :: String
source2 = "int aaa(void) {}"

expect2 :: Result
expect2 = Left $ NoMainDefined source2 0

source3 :: String
source3 = "void main(void) {}"

expect3 :: Result
expect3 = Left $ NoMainDefined source3 0

source4 :: String
source4 = "int main(int a) {}"

expect4 :: Result
expect4 = Left $ NoMainDefined source4 0

source5 :: String
source5 = unlines
    [ "int aaa(int a, int b) { int bbb; }"
    , "int bbb(int a, int b) { int aaa; }"
    ]

expect5 :: Result
expect5 = Right ()

source6 :: String
source6 = unlines
    [ "int aaa() {}"
    , "int aaa() {}"
    ]

expect6 :: Result
expect6 = Left $ IdentifierConfliction source6 19 "aaa"

source7 :: String
source7 = "int a; int a;"

expect7 :: Result
expect7 = Left $ IdentifierConfliction source7 11 "a"

source8 :: String
source8 = "int aaa(int a, int a) {}"

expect8 :: Result
expect8 = Left $ IdentifierConfliction source8 19 "a"

source9 :: String
source9 = "int aaa() { int a; int a; }"

expect9 :: Result
expect9 = Left $ IdentifierConfliction source9 23 "a"

source10 :: String
source10 = "int aaa(int a) { int a; }"

expect10 :: Result
expect10 = Left $ IdentifierConfliction source10 21 "a"

source11 :: String
source11 = "void aaa() {}"

expect11 :: Result
expect11 = Right ()

source12 :: String
source12 = "void a;"

expect12 :: Result
expect12 = Left $ InappropriateVarType source12 3 "void"

source13 :: String
source13 = "void aaa() { void a; }"

expect13 :: Result
expect13 = Left $ InappropriateVarType source13 16 "void"

source14 :: String
source14 = "void aaa() { for(;;) { for(;;) { for(;;) { void a; } } } }"

expect14 :: Result
expect14 = Left $ InappropriateVarType source14 46 "void"

source15 :: String
source15 = unlines
    [ "void aaa()"
    , "{"
    , "    for (;;)"
    , "    {"
    , "        continue;"
    , "        break;"
    , "        for (;;)"
    , "        {"
    , "            continue;"
    , "            break;"
    , "        }"
    , "    }"
    , "    while (1)"
    , "    {"
    , "        continue;"
    , "        break;"
    , "        if (a)"
    , "        {"
    , "            continue;"
    , "            break;"
    , "        }"
    , "    }"
    , "}"
    ]

expect15 :: Result
expect15 = Right ()

source16 :: String
source16 = unlines
    [ "void aaa()"
    , "{"
    , "    continue;"
    , "}"
    ]

expect16 :: Result
expect16 = Left $ LoopFeatureOutsideLoop source16 24 "continue"

source17 :: String
source17 = unlines
    [ "void aaa()"
    , "{"
    , "    if (a)"
    , "    {"
    , "        continue;"
    , "    }"
    , "}"
    ]

expect17 :: Result
expect17 = Left $ LoopFeatureOutsideLoop source17 45 "continue"

source18 :: String
source18 = unlines
    [ "void aaa()"
    , "{"
    , "    for (;;)"
    , "    {"
    , "        continue;"
    , "    }"
    , "    continue;"
    , "}"
    ]

expect18 :: Result
expect18 = Left $ LoopFeatureOutsideLoop source18 67 "continue"
