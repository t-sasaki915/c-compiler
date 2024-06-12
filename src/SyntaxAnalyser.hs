module SyntaxAnalyser
  ( SyntaxAnalyserError(..)
  , Syntax(..)
  , syntaxAnalyse
  ) where

import ExpressionAnalyser (Expression(..), ExpressionAnalyserError, expressionAnalyse)
import SourceFileAnalyser (sourceLoc)
import Tokeniser (Token(..))
import TokeniserDomain (typeKeywords)

data SyntaxAnalyserError = UnexpectedToken String Int Token String
                         | UnexpectedEOF String Int
                         | InvalidExpression ExpressionAnalyserError
                         deriving Eq

instance Show SyntaxAnalyserError where
    show (UnexpectedToken src ind t e) =
        "(" ++ sourceLoc src ind ++ ") Expected " ++ e ++ " but " ++ show t ++ " found."
    show (UnexpectedEOF src ind) =
        "(" ++ sourceLoc src ind ++ ") Unexpected end of file."
    show (InvalidExpression e) =
        show e

data Syntax = Program [Syntax]
            | Definitions [Syntax]
            | FunDefinition (Int, Token) (Int, Token) [Syntax] [Syntax]
            | VarDefinition (Int, Token) (Int, Token) (Maybe Expression)
            | VarReassign (Int, Token) Expression
            | FunctionCallSyntax (Int, Token) [Expression]
            | Return Expression
            | While Expression [Syntax]
            | Continue
            | Break
            | If Expression [Syntax] [Syntax]
            deriving (Show, Eq)

type IToken = (Int, Token)
type Expr = Expression

data AnalyserStep = ExpectVarOrFunType
                  | ExpectVarOrFunLabel IToken
                  | ExpectEqualOrOpenParenthesesOrSemicolon IToken IToken
                  | ExpectGlobalVariableDefaultValue IToken IToken
                  | ExpectGlobalVariableSemicolon IToken IToken Expr
                  | ExpectFunArgTypeOrEnd IToken IToken
                  | ExpectFunArgEnd IToken IToken
                  | ExpectFunArgType IToken IToken [Syntax]
                  | ExpectFunArgLabel IToken IToken IToken [Syntax]
                  | ExpectFunArgCommaOrEnd IToken IToken IToken IToken [Syntax]
                  | ExpectFunOpenBrace IToken IToken [Syntax]
                  | ExpectFunCloseBrace IToken IToken [Syntax] [Syntax]

syntaxAnalyse :: String -> [(Int, Token)] -> Either SyntaxAnalyserError Syntax
syntaxAnalyse source tokens = analyse ExpectVarOrFunType [] 0
    where
    analyse :: AnalyserStep -> [Syntax] -> Int -> Either SyntaxAnalyserError Syntax
    analyse step defs index | index >= length tokens =
        case step of
            ExpectVarOrFunType ->
                Right $ Program [ Definitions defs ]

            _ ->
                Left $ UnexpectedEOF source (fst $ last tokens)

    analyse step defs index =
        case step of
            ExpectVarOrFunType ->
                case token of
                    (_, Keyword k) | k `elem` typeKeywords ->
                        nextStep $ ExpectVarOrFunLabel token

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectVarOrFunLabel t)->
                case token of
                    (_, Identifier _) ->
                        nextStep $ ExpectEqualOrOpenParenthesesOrSemicolon t token

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectEqualOrOpenParenthesesOrSemicolon t l) ->
                case token of
                    (_, Semicolon) ->
                        determine $ VarDefinition t l Nothing

                    (_, OpenParentheses) ->
                        nextStep $ ExpectFunArgTypeOrEnd t l

                    (_, Symbol '=') ->
                        nextStep $ ExpectGlobalVariableDefaultValue t l

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectGlobalVariableDefaultValue t l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        nextStep' (ExpectGlobalVariableSemicolon t l expr) newIndex

                    Left err ->
                        Left $ InvalidExpression err

            (ExpectGlobalVariableSemicolon t l d) ->
                case token of
                    (_, Semicolon) ->
                        determine $ VarDefinition t l (Just d)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunArgTypeOrEnd t l) ->
                case token of
                    (_, Keyword "void") ->
                        nextStep $ ExpectFunArgEnd t l

                    (_, Keyword k) | k `elem` typeKeywords ->
                        nextStep $ ExpectFunArgLabel t l token []

                    (_, CloseParentheses) ->
                        nextStep $ ExpectFunOpenBrace t l []

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunArgEnd t l) ->
                case token of
                    (_, CloseParentheses) ->
                        nextStep $ ExpectFunOpenBrace t l []

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunArgType t l determined) ->
                case token of
                    (_, Keyword k) | k `elem` typeKeywords ->
                        nextStep $ ExpectFunArgLabel t l token determined

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunArgLabel t l at determined) ->
                case token of
                    (_, Identifier _) ->
                        nextStep $ ExpectFunArgCommaOrEnd t l at token determined

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunArgCommaOrEnd t l at al determined) ->
                case token of
                    (_, Comma) ->
                        let newArg = VarDefinition at al Nothing in
                        nextStep $ ExpectFunArgType t l (determined ++ [newArg])

                    (_, CloseParentheses) ->
                        let newArg = VarDefinition at al Nothing in
                        nextStep $ ExpectFunOpenBrace t l (determined ++ [newArg])

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunOpenBrace t l args) ->
                case token of
                    (_, OpenBrace) ->
                        case functionAnalyse source tokens False False (index + 1) of
                            Right (newIndex, contents) ->
                                nextStep' (ExpectFunCloseBrace t l args contents) newIndex
                                
                            Left err ->
                                Left err

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunCloseBrace t l args contents) ->
                case token of
                    (_, CloseBrace) ->
                        determine $ FunDefinition t l args contents

                    _ ->
                        contextualUnexpectedTokenHalt
        where
        token = tokens !! index

        nextStep' :: AnalyserStep -> Int -> Either SyntaxAnalyserError Syntax
        nextStep' newStep = analyse newStep defs
        nextStep :: AnalyserStep -> Either SyntaxAnalyserError Syntax
        nextStep newStep = nextStep' newStep (index + 1)
        determine' :: Syntax -> Int -> Either SyntaxAnalyserError Syntax
        determine' newSyntax = analyse ExpectVarOrFunType (defs ++ [newSyntax])
        determine :: Syntax -> Either SyntaxAnalyserError Syntax
        determine newSyntax = determine' newSyntax (index + 1)

        contextualUnexpectedTokenHalt :: Either SyntaxAnalyserError Syntax
        contextualUnexpectedTokenHalt =
            Left $ uncurry (UnexpectedToken source) token expectation
            where
            expectation =
                case step of
                    ExpectVarOrFunType ->
                        "Type"
                    (ExpectVarOrFunLabel {}) ->
                        "Identifier"
                    (ExpectEqualOrOpenParenthesesOrSemicolon {}) ->
                        "'=', '(' or ';'"
                    (ExpectGlobalVariableDefaultValue {}) ->
                        "Expression"
                    (ExpectGlobalVariableSemicolon {}) ->
                        "';'"
                    (ExpectFunArgTypeOrEnd {}) ->
                        "Type or ')'"
                    (ExpectFunArgEnd {}) ->
                        "')'"
                    (ExpectFunArgType {}) ->
                        "Type"
                    (ExpectFunArgLabel {}) ->
                        "Identifier"
                    (ExpectFunArgCommaOrEnd {}) ->
                        "',' or ')'"
                    (ExpectFunOpenBrace {}) ->
                        "'{'"
                    (ExpectFunCloseBrace {}) ->
                        "'}'"

data FAnalyseStep = ExpectFirstFactor
                  | ExpectReturnExpression
                  | ExpectReturnSemicolon Expr
                  | ExpectLocalVariableLabel IToken
                  | ExpectLocalVariableEqualOrSemicolon IToken IToken
                  | ExpectLocalVariableExpression IToken IToken
                  | ExpectLocalVariableSemicolon IToken IToken Expr
                  | ExpectEqualOrOpenParentheses IToken
                  | ExpectExpressionToReassignVariable IToken
                  | ExpectReassignSemicolon IToken Expr
                  | ExpectFunctionCallSemicolon IToken [Expression]
                  | ExpectWhileOpenParentheses
                  | ExpectWhileCondition
                  | ExpectWhileCloseParentheses Expression
                  | ExpectWhileOpenBraceOrSyntax Expression
                  | ExpectWhileCloseBrace Expression [Syntax]
                  | ExpectContinueSemicolon
                  | ExpectBreakSemicolon
                  | ExpectIfOpenParentheses
                  | ExpectIfCondition
                  | ExpectIfCloseParentheses Expression
                  | ExpectIfOpenBraceOrSyntax Expression
                  | ExpectIfCloseBrace Expression [Syntax]
                  | ExpectElseOrEnd Expression [Syntax]
                  | ExpectElseOpenBraceOrSyntax Expression [Syntax]
                  | ExpectElseCloseBrace Expression [Syntax] [Syntax]

functionAnalyse :: String -> [(Int, Token)] -> Bool -> Bool -> Int ->
                   Either SyntaxAnalyserError (Int, [Syntax])
functionAnalyse source tokens justOneSyntax insideLoop = analyse ExpectFirstFactor []
    where
    analyse :: FAnalyseStep -> [Syntax] -> Int ->
               Either SyntaxAnalyserError (Int, [Syntax])
    analyse _ _ index | index >= length tokens =
        Left $ UnexpectedEOF source (fst $ last tokens)

    analyse step contents index =
        case step of
            ExpectFirstFactor | justOneSyntax && not (null contents) ->
                Right (index - 1, contents)

            ExpectFirstFactor ->
                case token of
                    (_, Keyword "return") ->
                        nextStep ExpectReturnExpression

                    (_, Keyword "while") ->
                        nextStep ExpectWhileOpenParentheses

                    (_, Keyword "if") ->
                        nextStep ExpectIfOpenParentheses

                    (_, Keyword "continue") | insideLoop ->
                        nextStep ExpectContinueSemicolon

                    (_, Keyword "break") | insideLoop ->
                        nextStep ExpectBreakSemicolon

                    (_, Keyword k) | k `elem` typeKeywords ->
                        nextStep $ ExpectLocalVariableLabel token

                    (_, Identifier _) ->
                        nextStep $ ExpectEqualOrOpenParentheses token

                    (_, CloseBrace) ->
                        Right (index, contents)

                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectReturnExpression ->
                case token of
                    (_, Semicolon) ->
                        determine $ Return Void

                    _ ->
                        case expressionAnalyse source tokens index of
                            Right (newIndex, expr) ->
                                nextStep' (ExpectReturnSemicolon expr) newIndex

                            Left err ->
                                Left $ InvalidExpression err

            (ExpectReturnSemicolon e) ->
                case token of
                    (_, Semicolon) ->
                        determine $ Return e

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectLocalVariableLabel t) ->
                case token of
                    (_, Identifier _) ->
                        nextStep $ ExpectLocalVariableEqualOrSemicolon t token

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectLocalVariableEqualOrSemicolon t l) ->
                case token of
                    (_, Symbol '=') ->
                        nextStep $ ExpectLocalVariableExpression t l

                    (_, Semicolon) ->
                        determine $ VarDefinition t l Nothing

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectLocalVariableExpression t l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        nextStep' (ExpectLocalVariableSemicolon t l expr) newIndex

                    Left err ->
                        Left $ InvalidExpression err

            (ExpectLocalVariableSemicolon t l e) ->
                case token of
                    (_, Semicolon) ->
                        determine $ VarDefinition t l (Just e)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectEqualOrOpenParentheses l) ->
                case token of
                    (_, Symbol '=') ->
                        nextStep $ ExpectExpressionToReassignVariable l

                    (_, OpenParentheses) ->
                        case functionArgAnalyse source tokens (index + 1) of
                            Right (newIndex, args) ->
                                nextStep'
                                    (ExpectFunctionCallSemicolon l args) (newIndex + 1)

                            Left err ->
                                Left err

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectExpressionToReassignVariable l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        nextStep' (ExpectReassignSemicolon l expr) newIndex

                    Left err ->
                        Left $ InvalidExpression err

            (ExpectReassignSemicolon l e) ->
                case token of
                    (_, Semicolon) ->
                        determine $ VarReassign l e

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunctionCallSemicolon l args) ->
                case token of
                    (_, Semicolon) ->
                        determine $ FunctionCallSyntax l args

                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectWhileOpenParentheses ->
                case token of
                    (_, OpenParentheses) ->
                        nextStep ExpectWhileCondition

                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectWhileCondition ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        nextStep' (ExpectWhileCloseParentheses expr) newIndex

                    Left err ->
                        Left $ InvalidExpression err

            (ExpectWhileCloseParentheses cond) ->
                case token of
                    (_, CloseParentheses) ->
                        nextStep $ ExpectWhileOpenBraceOrSyntax cond

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectWhileOpenBraceOrSyntax cond) ->
                case token of
                    (_, OpenBrace) ->
                        case functionAnalyse source tokens False True (index + 1) of
                            Right (newIndex, inner) ->
                                nextStep' (ExpectWhileCloseBrace cond inner) newIndex

                            Left err ->
                                Left err
                    _ ->
                        case functionAnalyse source tokens True True index of
                            Right (newIndex, inner) ->
                                determine' (While cond inner) (newIndex + 1)

                            Left err ->
                                Left err

            (ExpectWhileCloseBrace cond inner) ->
                case token of
                    (_, CloseBrace) ->
                        determine $ While cond inner

                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectContinueSemicolon ->
                case token of
                    (_, Semicolon) ->
                        determine Continue
                    
                    _ -> 
                        contextualUnexpectedTokenHalt

            ExpectBreakSemicolon ->
                case token of
                    (_, Semicolon) ->
                        determine Break

                    _ ->
                        contextualUnexpectedTokenHalt
            
            ExpectIfOpenParentheses ->
                case token of
                    (_, OpenParentheses) ->
                        nextStep ExpectIfCondition
                    
                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectIfCondition ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        nextStep' (ExpectIfCloseParentheses expr) newIndex

                    Left err ->
                        Left $ InvalidExpression err

            (ExpectIfCloseParentheses cond) ->
                case token of
                    (_, CloseParentheses) ->
                        nextStep $ ExpectIfOpenBraceOrSyntax cond

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectIfOpenBraceOrSyntax cond) ->
                case token of
                    (_, OpenBrace) ->
                        case functionAnalyse source tokens False False (index + 1) of
                            Right (newIndex, inner) ->
                                nextStep' (ExpectIfCloseBrace cond inner) newIndex

                            Left err ->
                                Left err

                    _ ->
                        case functionAnalyse source tokens True False index of
                            Right (newIndex, inner) ->
                                nextStep' (ExpectElseOrEnd cond inner) (newIndex + 1)

                            Left err ->
                                Left err

            (ExpectIfCloseBrace cond inner) ->
                case token of
                    (_, CloseBrace) ->
                        nextStep $ ExpectElseOrEnd cond inner
                    
                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectElseOrEnd cond inner) ->
                case token of
                    (_, Keyword "else") ->
                        nextStep $ ExpectElseOpenBraceOrSyntax cond inner

                    _ ->
                        determine' (If cond inner []) index

            (ExpectElseOpenBraceOrSyntax cond inner) ->
                case token of
                    (_, OpenBrace) ->
                        case functionAnalyse source tokens False False (index + 1) of
                            Right (newIndex, elseInner) ->
                                nextStep'
                                    (ExpectElseCloseBrace cond inner elseInner) newIndex

                            Left err ->
                                Left err
                    
                    _ ->
                        case functionAnalyse source tokens True False index of
                            Right (newIndex, elseInner) ->
                                determine' (If cond inner elseInner) (newIndex + 1)

                            Left err ->
                                Left err

            (ExpectElseCloseBrace cond inner elseInner) ->
                case token of
                    (_, CloseBrace) ->
                        determine $ If cond inner elseInner

                    _ ->
                        contextualUnexpectedTokenHalt

        where
        token = tokens !! index

        nextStep' :: FAnalyseStep -> Int -> Either SyntaxAnalyserError (Int, [Syntax])
        nextStep' newStep = analyse newStep contents
        nextStep :: FAnalyseStep -> Either SyntaxAnalyserError (Int, [Syntax])
        nextStep newStep = nextStep' newStep (index + 1)
        determine' :: Syntax -> Int -> Either SyntaxAnalyserError (Int, [Syntax])
        determine' newContent = analyse ExpectFirstFactor (contents ++ [newContent])
        determine :: Syntax -> Either SyntaxAnalyserError (Int, [Syntax])
        determine newContent = determine' newContent (index + 1)

        contextualUnexpectedTokenHalt :: Either SyntaxAnalyserError (Int, [Syntax])
        contextualUnexpectedTokenHalt =
            Left $ uncurry (UnexpectedToken source) token expectation
            where
            expectation =
                case step of
                    ExpectFirstFactor ->
                        "Keyword, Identifier or '}'"
                    ExpectReturnExpression ->
                        "Expression or ';'"
                    (ExpectReturnSemicolon _) ->
                        "';'"
                    (ExpectLocalVariableLabel _) ->
                        "Identifier"
                    (ExpectLocalVariableEqualOrSemicolon {}) ->
                        "'=' or ';'"
                    (ExpectLocalVariableExpression {}) ->
                        "Expression"
                    (ExpectLocalVariableSemicolon {}) ->
                        "';'"
                    (ExpectEqualOrOpenParentheses _) ->
                        "'=' or '('"
                    (ExpectExpressionToReassignVariable _) ->
                        "Expression"
                    (ExpectReassignSemicolon {}) ->
                        "';'"
                    (ExpectFunctionCallSemicolon {}) ->
                        "';'"
                    ExpectWhileOpenParentheses ->
                        "'('"
                    ExpectWhileCondition ->
                        "Expression"
                    (ExpectWhileCloseParentheses _) ->
                        "')'"
                    (ExpectWhileOpenBraceOrSyntax _) ->
                        "'{' or Body"
                    (ExpectWhileCloseBrace _ _) ->
                        "'}'"
                    ExpectContinueSemicolon ->
                        "';'"
                    ExpectBreakSemicolon ->
                        "';'"
                    ExpectIfOpenParentheses ->
                        "'('"
                    ExpectIfCondition ->
                        "Expression"
                    (ExpectIfCloseParentheses _) ->
                        "')'"
                    (ExpectIfOpenBraceOrSyntax _) ->
                        "'{' or Body"
                    (ExpectIfCloseBrace _ _) ->
                        "'}'"
                    (ExpectElseOrEnd _ _) ->
                        "'else' or Body"
                    (ExpectElseOpenBraceOrSyntax _ _) ->
                        "'{' or Body"
                    (ExpectElseCloseBrace {}) ->
                        "'}'"

data AAnalyseStep = ExpectArgumentOrEnd
                  | ExpectCommaOrEnd
                  | ExpectArgument

functionArgAnalyse :: String -> [(Int, Token)] -> Int ->
                      Either SyntaxAnalyserError (Int, [Expression])
functionArgAnalyse source tokens = analyse ExpectArgumentOrEnd []
    where
    analyse :: AAnalyseStep -> [Expression] -> Int ->
               Either SyntaxAnalyserError (Int, [Expression])
    analyse _ _ index | index >= length tokens =
        Left $ UnexpectedEOF source (fst $ last tokens)

    analyse step args index =
        case step of
            ExpectArgumentOrEnd ->
                case token of
                    (_, CloseParentheses) ->
                        Right (index, [])

                    _ ->
                        case expressionAnalyse source tokens index of
                            Right (newIndex, expr) ->
                                analyse ExpectCommaOrEnd [expr] newIndex
                            Left err ->
                                Left $ InvalidExpression err

            ExpectCommaOrEnd ->
                case token of
                    (_, CloseParentheses) ->
                        Right (index, args)

                    (_, Comma) ->
                        analyse ExpectArgument args (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectArgument ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        analyse ExpectCommaOrEnd (args ++ [expr]) newIndex
                    Left err ->
                        Left $ InvalidExpression err

        where
        token = tokens !! index

        contextualUnexpectedTokenHalt :: Either SyntaxAnalyserError (Int, [Expression])
        contextualUnexpectedTokenHalt =
            Left $ uncurry (UnexpectedToken source) token expectation
            where
            expectation =
                case step of
                    ExpectArgumentOrEnd ->
                        "Expression or ')'"
                    ExpectCommaOrEnd ->
                        "',' or ')'"
                    ExpectArgument ->
                        "Expression"
