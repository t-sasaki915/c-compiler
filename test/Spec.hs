import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import ExpressionAnalyserSpec
import SemanticVerifierSpec
import SyntaxAnalyserSpec
import TokeniserSpec

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 || errors result > 0 then
        exitFailure
    
    else
        exitSuccess
    where
    tests = TestList
        [ TestLabel "Basic C Source Tokenisation #1"              tokeniserTest1
        , TestLabel "Basic C Source Tokenisation #2"              tokeniserTest2
        , TestLabel "Single-line Comment Tokenisation"            tokeniserTest3
        , TestLabel "Multi-line Comment Tokenisation"             tokeniserTest4
        , TestLabel "Unrecognisable Source Tokenisation"          tokeniserTest5
        , TestLabel "Special Symbol Tokenisation"                 tokeniserTest6
        , TestLabel "Invalid Special Symbol Tokenisation"         tokeniserTest7

        , TestLabel "Variable Reference Analysation"              expressionAnalyserTest1
        , TestLabel "Number Reference Analysation"                expressionAnalyserTest2
        , TestLabel "Redundant Parentheses Analysation"           expressionAnalyserTest3
        , TestLabel "Nested Redundant Parentheses Analysation"    expressionAnalyserTest4
        , TestLabel "No Argument Function Call Analysation"       expressionAnalyserTest5
        , TestLabel "Single Argument Function Call Analysation"   expressionAnalyserTest6
        , TestLabel "Multi Argument Function Call Analysation"    expressionAnalyserTest7
        , TestLabel "Nested Function Call Analysation"            expressionAnalyserTest8
        , TestLabel "Simple Addition Analysation"                 expressionAnalyserTest9
        , TestLabel "Addition of Three Variables Analysation"     expressionAnalyserTest10
        , TestLabel "Addition with Parentheses Analysation #1"    expressionAnalyserTest11
        , TestLabel "Addition with Parentheses Analysation #2"    expressionAnalyserTest12
        , TestLabel "Complex Equation Analysation"                expressionAnalyserTest13
        , TestLabel "Endless Expression Analysation"              expressionAnalyserTest14
        , TestLabel "Unrecognisable Expression Analysation"       expressionAnalyserTest15
        , TestLabel "Simple Opposition Analysation"               expressionAnalyserTest16
        , TestLabel "Nested Opposition Analysation"               expressionAnalyserTest17
        , TestLabel "Expression Opposition Analysation"           expressionAnalyserTest18
        , TestLabel "Function Opposition Analysation"             expressionAnalyserTest19
        , TestLabel "Complex Opposition Analysation"              expressionAnalyserTest20

        , TestLabel "Uninitialised Global Variable Analysation"   syntaxAnalyserTest1
        , TestLabel "Initialised Global Variable Analysation"     syntaxAnalyserTest2
        , TestLabel "No Argument Function Analysation"            syntaxAnalyserTest3
        , TestLabel "Function with Arguments Analysation"         syntaxAnalyserTest4
        , TestLabel "Return Syntax Analysation"                   syntaxAnalyserTest5
        , TestLabel "No Expression Return Analysation"            syntaxAnalyserTest6
        , TestLabel "Local Variable Analysation"                  syntaxAnalyserTest7
        , TestLabel "Variable Reassign Analysation"               syntaxAnalyserTest8
        , TestLabel "Function Call In Function Analysation"       syntaxAnalyserTest9
        , TestLabel "While Syntax Analysation"                    syntaxAnalyserTest10
        , TestLabel "Braces Reduced While Syntax Analysation"     syntaxAnalyserTest11
        , TestLabel "If-Else Syntax Analysation"                  syntaxAnalyserTest12
        , TestLabel "Braces Reduced If-Else Syntax Analysation"   syntaxAnalyserTest13
        , TestLabel "Syntax Sugar += and -= Analysation"          syntaxAnalyserTest14
        , TestLabel "Syntax Sugar ++ and -- Analysation #1"       syntaxAnalyserTest15
        , TestLabel "Syntax Sugar ++ and -- Analysation #2"       syntaxAnalyserTest16
        , TestLabel "For Syntax Analysation"                      syntaxAnalyserTest17
        , TestLabel "Endless For Syntax Analysation"              syntaxAnalyserTest18

        , TestLabel "Existence of The Entrypoint Verification #1" semanticVerifyTest1
        , TestLabel "Existence of The Entrypoint Verification #2" semanticVerifyTest2
        , TestLabel "Existence of The Entrypoint Verficiation #3" semanticVerifyTest3
        , TestLabel "Existence of The Entrypoint Verification #4" semanticVerifyTest4
        , TestLabel "Identifier Confliction Verification #1"      semanticVerifyTest5
        , TestLabel "Identifier Confliction Verification #2"      semanticVerifyTest6
        , TestLabel "Identifier Confliction Verification #3"      semanticVerifyTest7
        , TestLabel "Identifier Confliction Verification #4"      semanticVerifyTest8
        , TestLabel "Identifier Confliction Verification #5"      semanticVerifyTest9
        , TestLabel "Identifier Confliction Verification #6"      semanticVerifyTest10
        , TestLabel "Variable Type Verification #1"               semanticVerifyTest11
        , TestLabel "Variable Type Verification #2"               semanticVerifyTest12
        , TestLabel "Variable Type Verification #3"               semanticVerifyTest13
        , TestLabel "Variable Type Verification #4"               semanticVerifyTest14
        , TestLabel "Loop Feature Verification #1"                semanticVerifyTest15
        , TestLabel "Loop Feature Verification #2"                semanticVerifyTest16
        , TestLabel "Loop Feature Verification #3"                semanticVerifyTest17
        , TestLabel "Loop Feature Verification #4"                semanticVerifyTest18
        , TestLabel "Identifier Reference Verification #1"        semanticVerifyTest19
        , TestLabel "Identifier Reference Verification #2"        semanticVerifyTest20
        , TestLabel "Identifier Reference Verification #3"        semanticVerifyTest21
        , TestLabel "Identifier Reference Verification #4"        semanticVerifyTest22
        , TestLabel "Identifier Reference Verification #5"        semanticVerifyTest23
        , TestLabel "Identifier Reference Verification #6"        semanticVerifyTest24
        , TestLabel "Identifier Reference Verification #7"        semanticVerifyTest25
        , TestLabel "Identifier Reference Verification #8"        semanticVerifyTest26
        , TestLabel "Identifier Reference Verification #9"        semanticVerifyTest27
        ]
