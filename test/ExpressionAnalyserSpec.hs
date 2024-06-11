module ExpressionAnalyserSpec
    ( expressionAnalyserTest1
    , expressionAnalyserTest2
    , expressionAnalyserTest3
    , expressionAnalyserTest4
    , expressionAnalyserTest5
    , expressionAnalyserTest6
    , expressionAnalyserTest7
    , expressionAnalyserTest8
    , expressionAnalyserTest9
    , expressionAnalyserTest10
    , expressionAnalyserTest11
    , expressionAnalyserTest12
    , expressionAnalyserTest13
    , expressionAnalyserTest14
    , expressionAnalyserTest15
    , expressionAnalyserTest16
    , expressionAnalyserTest17
    , expressionAnalyserTest18
    , expressionAnalyserTest19
    , expressionAnalyserTest20
    ) where

import ExpressionAnalyser
import ExpressionAnalyserSpecDomain
import Tokeniser (tokenise)

import Test.HUnit

expressionAnalyserTest :: String -> Result -> Test
expressionAnalyserTest src expect =
    case tokenise src of
        Right tokens ->
            TestCase $ assertEqual "" (expressionAnalyse src tokens 0) expect
        Left _ ->
            TestCase $ assertEqual "" True False

expressionAnalyserTest1 :: Test
expressionAnalyserTest1 = expressionAnalyserTest source1 expect1

expressionAnalyserTest2 :: Test
expressionAnalyserTest2 = expressionAnalyserTest source2 expect2

expressionAnalyserTest3 :: Test
expressionAnalyserTest3 = expressionAnalyserTest source3 expect3

expressionAnalyserTest4 :: Test
expressionAnalyserTest4 = expressionAnalyserTest source4 expect4

expressionAnalyserTest5 :: Test
expressionAnalyserTest5 = expressionAnalyserTest source5 expect5

expressionAnalyserTest6 :: Test
expressionAnalyserTest6 = expressionAnalyserTest source6 expect6

expressionAnalyserTest7 :: Test
expressionAnalyserTest7 = expressionAnalyserTest source7 expect7

expressionAnalyserTest8 :: Test
expressionAnalyserTest8 = expressionAnalyserTest source8 expect8

expressionAnalyserTest9 :: Test
expressionAnalyserTest9 = expressionAnalyserTest source9 expect9

expressionAnalyserTest10 :: Test
expressionAnalyserTest10 = expressionAnalyserTest source10 expect10

expressionAnalyserTest11 :: Test
expressionAnalyserTest11 = expressionAnalyserTest source11 expect11

expressionAnalyserTest12 :: Test
expressionAnalyserTest12 = expressionAnalyserTest source12 expect12

expressionAnalyserTest13 :: Test
expressionAnalyserTest13 = expressionAnalyserTest source13 expect13

expressionAnalyserTest14 :: Test
expressionAnalyserTest14 = expressionAnalyserTest source14 expect14

expressionAnalyserTest15 :: Test
expressionAnalyserTest15 = expressionAnalyserTest source15 expect15

expressionAnalyserTest16 :: Test
expressionAnalyserTest16 = expressionAnalyserTest source16 expect16

expressionAnalyserTest17 :: Test
expressionAnalyserTest17 = expressionAnalyserTest source17 expect17

expressionAnalyserTest18 :: Test
expressionAnalyserTest18 = expressionAnalyserTest source18 expect18

expressionAnalyserTest19 :: Test
expressionAnalyserTest19 = expressionAnalyserTest source19 expect19

expressionAnalyserTest20 :: Test
expressionAnalyserTest20 = expressionAnalyserTest source20 expect20
