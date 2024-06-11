module ExpressionAnalyser
    ( Expression(..)
    , ExpressionAnalyserError(..)
    , expressionAnalyse
    ) where

import SourceFileAnalyser (sourceLoc)
import Tokeniser (Token(..))

data ExpressionAnalyserError = UnexpectedToken String Int Token String
                             | UnexpectedEOF String Int
                             deriving Eq

instance Show ExpressionAnalyserError where
    show (UnexpectedToken src ind t e) =
        "(" ++ sourceLoc src ind ++ ") Expected " ++ e ++ " but " ++ show t ++ " found."
    show (UnexpectedEOF src ind) =
        "(" ++ sourceLoc src ind ++ ") Unexpected end of file."

data Expression = VarReference (Int, Token)
                | NumReference (Int, Token)
                | FunctionCall (Int, Token) [Expression]
                | Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                | Equals Expression Expression
                | NotEquals Expression Expression
                | MoreThan Expression Expression
                | LessThan Expression Expression
                | MoreThanOrEq Expression Expression
                | LessThanOrEq Expression Expression
                | Opposition Expression
                deriving (Show, Eq)

data AnalyserStep = ExpectOpenParenthesesOrIdentifierOrNumber
                  | ExpectExpressionToReverse
                  | ExpectCalcSymbolOrEnd Expression
                  | ExpectExpressionToDoBasicCalculation
                        (Expression -> Expression -> Expression)
                        Expression

expressionAnalyse :: String -> [(Int, Token)] -> Int ->
                     Either ExpressionAnalyserError (Int, Expression)
expressionAnalyse source tokens =
    analyse ExpectOpenParenthesesOrIdentifierOrNumber
    where
    analyse :: AnalyserStep -> Int -> Either ExpressionAnalyserError (Int, Expression)
    analyse _ index | index >= length tokens =
        Left $ UnexpectedEOF source (fst $ last tokens)

    analyse step index =
        case step of
            ExpectOpenParenthesesOrIdentifierOrNumber ->
                case token of
                    (_, OpenParentheses) ->
                        case expressionAnalyse source tokens (index + 1) of
                            Right (newIndex, expr) ->
                                let newStep = ExpectCalcSymbolOrEnd expr in
                                analyse newStep (newIndex + 1)
                            Left err ->
                                Left err

                    (_, Identifier _) ->
                        case functionCallAnalyse source tokens index of
                            Right (newIndex, expr) ->
                                let newStep = ExpectCalcSymbolOrEnd expr in
                                analyse newStep (newIndex + 1)
                            Left err ->
                                Left err

                    (_, Number _) ->
                        let expr = NumReference token
                            newStep = ExpectCalcSymbolOrEnd expr in
                        analyse newStep (index + 1)

                    (_, Symbol '!') ->
                        let newStep = ExpectExpressionToReverse in
                        analyse newStep (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectExpressionToReverse ->
                case token of
                    (_, OpenParentheses) ->
                        case expressionAnalyse source tokens (index + 1) of
                            Right (newIndex, expr) ->
                                let newExpr = Opposition expr
                                    newStep = ExpectCalcSymbolOrEnd newExpr in
                                analyse newStep (newIndex + 1)
                            Left err ->
                                Left err

                    (_, Identifier _) ->
                        case functionCallAnalyse source tokens index of
                            Right (newIndex, expr) ->
                                let newExpr = Opposition expr
                                    newStep = ExpectCalcSymbolOrEnd newExpr in
                                analyse newStep (newIndex + 1)
                            Left err ->
                                Left err

                    (_, Number _) ->
                        let expr = Opposition $ NumReference token
                            newStep = ExpectCalcSymbolOrEnd expr in
                        analyse newStep (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectCalcSymbolOrEnd e) ->
                case token of
                    (_, Semicolon) ->
                        Right (index, e)

                    (_, CloseParentheses) ->
                        Right (index, e)

                    (_, Comma) ->
                        Right (index, e)

                    (_, Equality) ->
                        scheduleBasicCalculation Equals e

                    (_, NotEquality) ->
                        scheduleBasicCalculation NotEquals e

                    (_, MoreOrEq) ->
                        scheduleBasicCalculation MoreThanOrEq e

                    (_, LessOrEq) ->
                        scheduleBasicCalculation LessThanOrEq e

                    (_, Symbol '+') ->
                        scheduleBasicCalculation Addition e

                    (_, Symbol '-') ->
                        scheduleBasicCalculation Subtraction e

                    (_, Symbol '*') ->
                        scheduleBasicCalculation Multiplication e

                    (_, Symbol '/') ->
                        scheduleBasicCalculation Division e

                    (_, Symbol '>') ->
                        scheduleBasicCalculation MoreThan e

                    (_, Symbol '<') ->
                        scheduleBasicCalculation LessThan e

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectExpressionToDoBasicCalculation calc e) ->
                basicCalculation calc e
        where
        token = tokens !! index

        scheduleBasicCalculation :: (Expression -> Expression -> Expression) ->
                                    Expression ->
                                    Either ExpressionAnalyserError (Int, Expression)
        scheduleBasicCalculation f e =
            let newStep = ExpectExpressionToDoBasicCalculation f e in
            analyse newStep (index + 1)

        basicCalculation :: (Expression -> Expression -> Expression) ->
                            Expression -> Either ExpressionAnalyserError (Int, Expression)
        basicCalculation f e =
            case token of
                (_, OpenParentheses) ->
                    case expressionAnalyse source tokens (index + 1) of
                        Right (newIndex, expr) ->
                            let newExpr = f e expr
                                newStep = ExpectCalcSymbolOrEnd newExpr in
                            analyse newStep (newIndex + 1)
                        Left err ->
                            Left err

                (_, Identifier _) ->
                    case functionCallAnalyse source tokens index of
                        Right (newIndex, expr) ->
                            let newExpr = f e expr
                                newStep = ExpectCalcSymbolOrEnd newExpr in
                            analyse newStep (newIndex + 1)
                        Left err ->
                            Left err

                (_, Number _) ->
                    let expr = f e (NumReference token)
                        newStep = ExpectCalcSymbolOrEnd expr in
                    analyse newStep (index + 1)

                _ ->
                    contextualUnexpectedTokenHalt

        contextualUnexpectedTokenHalt :: Either ExpressionAnalyserError (Int, Expression)
        contextualUnexpectedTokenHalt =
            Left $ uncurry (UnexpectedToken source) token expectation
            where
            expectation =
                case step of
                    ExpectOpenParenthesesOrIdentifierOrNumber ->
                        "'(', Identifier or Number"
                    ExpectExpressionToReverse ->
                        "Expression"
                    (ExpectCalcSymbolOrEnd _) ->
                        "Calculation Symbol, ')', ',' or ';'"
                    (ExpectExpressionToDoBasicCalculation _ _) ->
                        "Expression"


data FAnalyserStep = ExpectIdentifier
                   | ExpectOpenParentheses (Int, Token)
                   | ExpectArgOrCloseParentheses (Int, Token) [Expression]
                   | ExpectArgument (Int, Token) [Expression]
                   | ExpectCommaOrCloseParentheses (Int, Token) [Expression]

functionCallAnalyse :: String -> [(Int, Token)] -> Int ->
                       Either ExpressionAnalyserError (Int, Expression)
functionCallAnalyse source tokens = analyse ExpectIdentifier
    where
    analyse :: FAnalyserStep -> Int -> Either ExpressionAnalyserError (Int, Expression)
    analyse _ index | index >= length tokens =
        Left $ UnexpectedEOF source (fst $ last tokens)

    analyse step index =
        case step of
            ExpectIdentifier ->
                case token of
                    (_, Identifier _) ->
                        let newStep = ExpectOpenParentheses token in
                        analyse newStep (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectOpenParentheses t) ->
                case token of
                    (_, OpenParentheses) ->
                        let newStep = ExpectArgOrCloseParentheses t [] in
                        analyse newStep (index + 1)

                    _ ->
                        Right (index - 1, VarReference t)

            (ExpectArgOrCloseParentheses t determined) ->
                case token of
                    (_, CloseParentheses) ->
                        Right (index, FunctionCall t determined)

                    _ ->
                        case expressionAnalyse source tokens index of
                            Right (newIndex, expr) ->
                                let args = determined ++ [expr]
                                    newStep = ExpectCommaOrCloseParentheses t args in
                                analyse newStep newIndex
                            Left err ->
                                Left err

            (ExpectArgument t determined) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let args = determined ++ [expr]
                            newStep = ExpectCommaOrCloseParentheses t args in
                        analyse newStep newIndex
                    Left err ->
                        Left err

            (ExpectCommaOrCloseParentheses t determined) ->
                case token of
                    (_, Comma) ->
                        let newStep = ExpectArgument t determined in
                        analyse newStep (index + 1)

                    (_, CloseParentheses) ->
                        Right (index, FunctionCall t determined)

                    _ ->
                        contextualUnexpectedTokenHalt

        where
        token = tokens !! index

        contextualUnexpectedTokenHalt :: Either ExpressionAnalyserError (Int, Expression)
        contextualUnexpectedTokenHalt =
            Left $ uncurry (UnexpectedToken source) token expectation
            where
            expectation =
                case step of
                    ExpectIdentifier ->
                        "Identifier"
                    (ExpectOpenParentheses _) ->
                        "'('"
                    (ExpectArgOrCloseParentheses _ _) ->
                        "Expression or ')'"
                    (ExpectArgument _ _) ->
                        "Expression"
                    (ExpectCommaOrCloseParentheses _ _) ->
                        "',' or ')'"
