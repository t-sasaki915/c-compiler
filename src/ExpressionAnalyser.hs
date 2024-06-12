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
                | Negative Expression
                | And Expression Expression
                | Or Expression Expression
                | Void
                deriving (Show, Eq)

data AnalyserStep = ExpectOpenParenthesesOrIdentifierOrNumber
                  | ExpectExpressionToDoSingleExprCalculation
                        (Expression -> Expression)
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
                                nextStep' (ExpectCalcSymbolOrEnd expr) (newIndex + 1)

                            Left err ->
                                Left err

                    (_, Identifier _) ->
                        case functionCallAnalyse source tokens index of
                            Right (newIndex, expr) ->
                                nextStep' (ExpectCalcSymbolOrEnd expr) (newIndex + 1)

                            Left err ->
                                Left err

                    (_, Number _) ->
                        nextStep $ ExpectCalcSymbolOrEnd (NumReference token)

                    (_, Symbol '!') ->
                        scheduleSingleExprCalculation Opposition

                    (_, Symbol '-') ->
                        scheduleSingleExprCalculation Negative

                    _ ->
                        unexpectedTokenHalt "'(', '!', '-', Identifier or Number"

            (ExpectExpressionToDoSingleExprCalculation f) ->
                singleExprCalculation f

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

                    (_, AndAnd) ->
                        scheduleBasicCalculation And e

                    (_, BarBar) ->
                        scheduleBasicCalculation Or e

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
                        unexpectedTokenHalt "';', ')', ',' or Calculation Symbol"

            (ExpectExpressionToDoBasicCalculation calc e) ->
                basicCalculation calc e
        where
        token = tokens !! index

        nextStep' = analyse
        nextStep newStep = nextStep' newStep (index + 1)
        unexpectedTokenHalt e = Left $ uncurry (UnexpectedToken source) token e

        scheduleBasicCalculation :: (Expression -> Expression -> Expression) ->
                                    Expression ->
                                    Either ExpressionAnalyserError (Int, Expression)
        scheduleBasicCalculation f e =
            nextStep $ ExpectExpressionToDoBasicCalculation f e

        scheduleSingleExprCalculation :: (Expression -> Expression) ->
                                         Either ExpressionAnalyserError (Int, Expression)
        scheduleSingleExprCalculation f =
            nextStep $ ExpectExpressionToDoSingleExprCalculation f

        basicCalculation :: (Expression -> Expression -> Expression) ->
                            Expression -> Either ExpressionAnalyserError (Int, Expression)
        basicCalculation f e =
            case token of
                (_, OpenParentheses) ->
                    case expressionAnalyse source tokens (index + 1) of
                        Right (newIndex, expr) ->
                            nextStep' (ExpectCalcSymbolOrEnd (f e expr)) (newIndex + 1)

                        Left err ->
                            Left err

                (_, Identifier _) ->
                    case functionCallAnalyse source tokens index of
                        Right (newIndex, expr) ->
                            nextStep' (ExpectCalcSymbolOrEnd (f e expr)) (newIndex + 1)

                        Left err ->
                            Left err

                (_, Number _) ->
                    nextStep $ ExpectCalcSymbolOrEnd (f e (NumReference token))

                _ ->
                    unexpectedTokenHalt "'(', Identifier or Number"

        singleExprCalculation :: (Expression -> Expression) ->
                                 Either ExpressionAnalyserError (Int, Expression)
        singleExprCalculation f =
            case token of
                (_, OpenParentheses) ->
                    case expressionAnalyse source tokens (index + 1) of
                        Right (newIndex, expr) ->
                            nextStep' (ExpectCalcSymbolOrEnd (f expr)) (newIndex + 1)

                        Left err ->
                            Left err

                (_, Identifier _) ->
                    case functionCallAnalyse source tokens index of
                        Right (newIndex, expr) ->
                            nextStep' (ExpectCalcSymbolOrEnd (f expr)) (newIndex + 1)

                        Left err ->
                            Left err

                (_, Number _) ->
                    nextStep $ ExpectCalcSymbolOrEnd (f (NumReference token))

                _ ->
                    unexpectedTokenHalt "'(', Identifier or Number"


data FAnalyserStep = ExpectIdentifier
                   | ExpectOpenParentheses (Int, Token)
                   | ExpectArgOrCloseParentheses (Int, Token)
                   | ExpectArgument (Int, Token)
                   | ExpectCommaOrCloseParentheses (Int, Token)

functionCallAnalyse :: String -> [(Int, Token)] -> Int ->
                       Either ExpressionAnalyserError (Int, Expression)
functionCallAnalyse source tokens = analyse ExpectIdentifier []
    where
    analyse :: FAnalyserStep -> [Expression] -> Int ->
               Either ExpressionAnalyserError (Int, Expression)
    analyse _  _ index | index >= length tokens =
        Left $ UnexpectedEOF source (fst $ last tokens)

    analyse step args index =
        case step of
            ExpectIdentifier ->
                case token of
                    (_, Identifier _) ->
                        nextStep $ ExpectOpenParentheses token

                    _ ->
                        unexpectedTokenHalt "Identifier"

            (ExpectOpenParentheses t) ->
                case token of
                    (_, OpenParentheses) ->
                        nextStep $ ExpectArgOrCloseParentheses t

                    _ ->
                        Right (index - 1, VarReference t)

            (ExpectArgOrCloseParentheses t) ->
                case token of
                    (_, CloseParentheses) ->
                        Right (index, FunctionCall t args)

                    _ ->
                        case expressionAnalyse source tokens index of
                            Right (newIndex, expr) ->
                                determine expr (ExpectCommaOrCloseParentheses t) newIndex

                            Left err ->
                                Left err

            (ExpectArgument t) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        determine expr (ExpectCommaOrCloseParentheses t) newIndex

                    Left err ->
                        Left err

            (ExpectCommaOrCloseParentheses t) ->
                case token of
                    (_, Comma) ->
                        nextStep $ ExpectArgument t

                    (_, CloseParentheses) ->
                        Right (index, FunctionCall t args)

                    _ ->
                        unexpectedTokenHalt "',' or ')'"

        where
        token = tokens !! index

        nextStep' newStep = analyse newStep args
        nextStep newStep = nextStep' newStep (index + 1)
        determine newArg newStep = analyse newStep (args ++ [newArg])
        unexpectedTokenHalt e = Left $ uncurry (UnexpectedToken source) token e
