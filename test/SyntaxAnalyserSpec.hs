module SyntaxAnalyserSpec
    ( syntaxAnalyserTest1
    , syntaxAnalyserTest2
    , syntaxAnalyserTest3
    , syntaxAnalyserTest4
    , syntaxAnalyserTest5
    , syntaxAnalyserTest6
    , syntaxAnalyserTest7
    , syntaxAnalyserTest8
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
