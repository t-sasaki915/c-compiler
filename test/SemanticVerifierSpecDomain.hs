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
