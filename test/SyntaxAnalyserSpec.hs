module SyntaxAnalyserSpec
    ( syntaxAnalyserTest1
    , syntaxAnalyserTest2
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
