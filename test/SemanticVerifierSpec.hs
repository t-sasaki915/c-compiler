module SemanticVerifierSpec
    ( semanticVerifyTest1
    , semanticVerifyTest2
    , semanticVerifyTest3
    , semanticVerifyTest4
    , semanticVerifyTest5
    , semanticVerifyTest6
    , semanticVerifyTest7
    , semanticVerifyTest8
    , semanticVerifyTest9
    , semanticVerifyTest10
    , semanticVerifyTest11
    , semanticVerifyTest12
    , semanticVerifyTest13
    , semanticVerifyTest14
    ) where

import SemanticVerifier
import SemanticVerifierSpecDomain
import SyntaxAnalyser (Syntax, syntaxAnalyse)
import Tokeniser (tokenise)

import Test.HUnit

semanticVerifyTest :: (String -> Syntax -> Result) -> String -> Result -> Test
semanticVerifyTest f src expect =
    case tokenise src of
        Right tokens ->
            case syntaxAnalyse src tokens of
                Right program ->
                    TestCase $ assertEqual "" (f src program) expect
                
                Left _ ->
                    TestCase $ assertEqual "" True False
        
        Left _ ->
            TestCase $ assertEqual "" True False

semanticVerifyTest1 :: Test
semanticVerifyTest1 = semanticVerifyTest mainDetection source1 expect1

semanticVerifyTest2 :: Test
semanticVerifyTest2 = semanticVerifyTest mainDetection source2 expect2

semanticVerifyTest3 :: Test
semanticVerifyTest3 = semanticVerifyTest mainDetection source3 expect3

semanticVerifyTest4 :: Test
semanticVerifyTest4 = semanticVerifyTest mainDetection source4 expect4

semanticVerifyTest5 :: Test
semanticVerifyTest5 = semanticVerifyTest conflictionCheck source5 expect5

semanticVerifyTest6 :: Test
semanticVerifyTest6 = semanticVerifyTest conflictionCheck source6 expect6

semanticVerifyTest7 :: Test
semanticVerifyTest7 = semanticVerifyTest conflictionCheck source7 expect7

semanticVerifyTest8 :: Test
semanticVerifyTest8 = semanticVerifyTest conflictionCheck source8 expect8

semanticVerifyTest9 :: Test
semanticVerifyTest9 = semanticVerifyTest conflictionCheck source9 expect9

semanticVerifyTest10 :: Test
semanticVerifyTest10 = semanticVerifyTest conflictionCheck source10 expect10

semanticVerifyTest11 :: Test
semanticVerifyTest11 = semanticVerifyTest variableTypeCheck source11 expect11

semanticVerifyTest12 :: Test
semanticVerifyTest12 = semanticVerifyTest variableTypeCheck source12 expect12

semanticVerifyTest13 :: Test
semanticVerifyTest13 = semanticVerifyTest variableTypeCheck source13 expect13

semanticVerifyTest14 :: Test
semanticVerifyTest14 = semanticVerifyTest variableTypeCheck source14 expect14
