module TokeniserSpec
    ( tokeniserTest1
    , tokeniserTest2
    , tokeniserTest3
    , tokeniserTest4
    , tokeniserTest5
    , tokeniserTest6
    , tokeniserTest7
    ) where

import Tokeniser
import TokeniserSpecDomain

import Test.HUnit

tokeniserTest :: String -> Result -> Test
tokeniserTest src expect =
    TestCase $ assertEqual "" (tokenise src) expect

tokeniserTest1 :: Test
tokeniserTest1 = tokeniserTest source1 expect1

tokeniserTest2 :: Test
tokeniserTest2 = tokeniserTest source2 expect2

tokeniserTest3 :: Test
tokeniserTest3 = tokeniserTest source3 expect3

tokeniserTest4 :: Test
tokeniserTest4 = tokeniserTest source4 expect4

tokeniserTest5 :: Test
tokeniserTest5 = tokeniserTest source5 expect5

tokeniserTest6 :: Test
tokeniserTest6 = tokeniserTest source6 expect6

tokeniserTest7 :: Test
tokeniserTest7 = tokeniserTest source7 expect7
