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
