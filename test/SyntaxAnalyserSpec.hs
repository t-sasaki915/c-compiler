module SyntaxAnalyserSpec
    ( syntaxAnalyserTest1
    , syntaxAnalyserTest2
    , syntaxAnalyserTest3
    , syntaxAnalyserTest4
    , syntaxAnalyserTest5
    , syntaxAnalyserTest6
    , syntaxAnalyserTest7
    , syntaxAnalyserTest8
    , syntaxAnalyserTest9
    , syntaxAnalyserTest10
    , syntaxAnalyserTest11
    , syntaxAnalyserTest12
    , syntaxAnalyserTest13
    ) where

import SyntaxAnalyser
import SyntaxAnalyserSpecDomain
import Tokeniser (tokenise)

import Test.HUnit

syntaxAnalyserTest :: String -> Result -> Test
syntaxAnalyserTest src expect =
    case tokenise src of
        Right tokens ->
            TestCase $ assertEqual "" (syntaxAnalyse src tokens) expect
        
        Left _ ->
            TestCase $ assertEqual "" True False

syntaxAnalyserTest1 :: Test
syntaxAnalyserTest1 = syntaxAnalyserTest source1 expect1

syntaxAnalyserTest2 :: Test
syntaxAnalyserTest2 = syntaxAnalyserTest source2 expect2

syntaxAnalyserTest3 :: Test
syntaxAnalyserTest3 = syntaxAnalyserTest source3 expect3

syntaxAnalyserTest4 :: Test
syntaxAnalyserTest4 = syntaxAnalyserTest source4 expect4

syntaxAnalyserTest5 :: Test
syntaxAnalyserTest5 = syntaxAnalyserTest source5 expect5

syntaxAnalyserTest6 :: Test
syntaxAnalyserTest6 = syntaxAnalyserTest source6 expect6

syntaxAnalyserTest7 :: Test
syntaxAnalyserTest7 = syntaxAnalyserTest source7 expect7

syntaxAnalyserTest8 :: Test
syntaxAnalyserTest8 = syntaxAnalyserTest source8 expect8

syntaxAnalyserTest9 :: Test
syntaxAnalyserTest9 = syntaxAnalyserTest source9 expect9

syntaxAnalyserTest10 :: Test
syntaxAnalyserTest10 = syntaxAnalyserTest source10 expect10

syntaxAnalyserTest11 :: Test
syntaxAnalyserTest11 = syntaxAnalyserTest source11 expect11

syntaxAnalyserTest12 :: Test
syntaxAnalyserTest12 = syntaxAnalyserTest source12 expect12

syntaxAnalyserTest13 :: Test
syntaxAnalyserTest13 = syntaxAnalyserTest source13 expect13
