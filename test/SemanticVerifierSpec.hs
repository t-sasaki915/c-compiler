module SemanticVerifierSpec
    ( semanticVerifyTest1
    , semanticVerifyTest2
    , semanticVerifyTest3
    , semanticVerifyTest4
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
