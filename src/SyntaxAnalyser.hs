module SyntaxAnalyser
  ( SyntaxAnalyserError(..)
  , Syntax(..)
  , SyntaxTree(..)
  , syntaxAnalyse
  ) where

import ExpressionAnalyser (Expression(..), ExpressionAnalyserError, expressionAnalyse)
import SourceFileAnalyser (sourceLoc)
import Tokeniser (Token(..))
import TokeniserDomain (typeKeywords)

import Data.List (intercalate)

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

data Syntax = Program
            | FunDefinition (Int, Token) (Int, Token) [Syntax] [Syntax]
            | VarDefinition (Int, Token) (Int, Token) (Maybe Expression)
            | VarReassign (Int, Token) Expression
            | FunctionCallSyntax (Int, Token) [Expression]
            | Return Expression
            | While Expression [Syntax]
            | If Expression [Syntax] [Syntax]
            deriving (Show, Eq)

data SyntaxTree = SyntaxTree
    { _rootSyntax :: Syntax
    , _subSyntax :: [SyntaxTree]
    }
    deriving Eq

instance Show SyntaxTree where
    show (SyntaxTree root []) = show root
    show (SyntaxTree root subs) =
        show root ++ " [" ++ intercalate ", " (map show subs) ++ "]"

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

syntaxAnalyse :: String -> [(Int, Token)] -> Either SyntaxAnalyserError SyntaxTree
syntaxAnalyse source tokens = analyse ExpectVarOrFunType [] 0
    where
    analyse :: AnalyserStep -> [SyntaxTree] -> Int -> Either SyntaxAnalyserError SyntaxTree
    analyse step defs index | index >= length tokens =
        case step of
            ExpectVarOrFunType ->
                Right $ SyntaxTree Program defs

            _ ->
                Left $ UnexpectedEOF source (fst $ last tokens)

    analyse step defs index =
        case step of
            ExpectVarOrFunType ->
                case token of
                    (_, Keyword k) | k `elem` typeKeywords ->
                        let newStep = ExpectVarOrFunLabel token in
                        analyse newStep defs (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectVarOrFunLabel t)->
                case token of
                    (_, Identifier _) ->
                        let
                            newStep =
                                ExpectEqualOrOpenParenthesesOrSemicolon t token in
                        analyse newStep defs (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectEqualOrOpenParenthesesOrSemicolon t l) ->
                case token of
                    (_, Semicolon) ->
                        let newStep = ExpectVarOrFunType
                            newVar = SyntaxTree (VarDefinition t l Nothing) []
                            newDefs = defs ++ [newVar] in
                        analyse newStep newDefs (index + 1)

                    (_, OpenParentheses) ->
                        let newStep = ExpectFunArgTypeOrEnd t l in
                        analyse newStep defs (index + 1)

                    (_, Symbol '=') ->
                        let newStep = ExpectGlobalVariableDefaultValue t l in
                        analyse newStep defs (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectGlobalVariableDefaultValue t l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let newStep = ExpectGlobalVariableSemicolon t l expr in
                        analyse newStep defs newIndex
                    Left err ->
                        Left $ InvalidExpression err

            (ExpectGlobalVariableSemicolon t l d) ->
                case token of
                    (_, Semicolon) ->
                        let newStep = ExpectVarOrFunType
                            newVar = SyntaxTree (VarDefinition t l (Just d)) []
                            newDefs = defs ++ [newVar] in
                        analyse newStep newDefs (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunArgTypeOrEnd t l) ->
                case token of
                    (_, Keyword "void") ->
                        let newStep = ExpectFunArgEnd t l in
                        analyse newStep defs (index + 1)

                    (_, Keyword k) | k `elem` typeKeywords ->
                        let newStep = ExpectFunArgLabel t l token [] in
                        analyse newStep defs (index + 1)

                    (_, CloseParentheses) ->
                        let newStep = ExpectFunOpenBrace t l [] in
                        analyse newStep defs (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunArgEnd t l) ->
                case token of
                    (_, CloseParentheses) ->
                        let newStep = ExpectFunOpenBrace t l [] in
                        analyse newStep defs (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunArgType t l determined) ->
                case token of
                    (_, Keyword k) | k `elem` typeKeywords ->
                        let newStep = ExpectFunArgLabel t l token determined in
                        analyse newStep defs (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunArgLabel t l at determined) ->
                case token of
                    (_, Identifier _) ->
                        let newStep = ExpectFunArgCommaOrEnd t l at token determined in
                        analyse newStep defs (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunArgCommaOrEnd t l at al determined) ->
                case token of
                    (_, Comma) ->
                        let newArg = VarDefinition at al Nothing
                            args = determined ++ [newArg]
                            newStep = ExpectFunArgType t l args in
                        analyse newStep defs (index + 1)

                    (_, CloseParentheses) ->
                        let newArg = VarDefinition at al Nothing
                            args = determined ++ [newArg]
                            newStep = ExpectFunOpenBrace t l args in
                        analyse newStep defs (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunOpenBrace t l args) ->
                case token of
                    (_, OpenBrace) ->
                        case functionAnalyse source tokens False (index + 1) of
                            Right (newIndex, contents) ->
                                let newStep = ExpectFunCloseBrace t l args contents in
                                analyse newStep defs newIndex
                            Left err ->
                                Left err

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunCloseBrace t l args contents) ->
                case token of
                    (_, CloseBrace) ->
                        let newFunc = SyntaxTree (FunDefinition t l args contents) []
                            newDefs = defs ++ [newFunc]
                            newStep = ExpectVarOrFunType in
                        analyse newStep newDefs (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt
        where
        token = tokens !! index

        contextualUnexpectedTokenHalt :: Either SyntaxAnalyserError SyntaxTree
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
                  | ExpectIfOpenParentheses
                  | ExpectIfCondition
                  | ExpectIfCloseParentheses Expression
                  | ExpectIfOpenBraceOrSyntax Expression
                  | ExpectIfCloseBrace Expression [Syntax]
                  | ExpectElseOrEnd Expression [Syntax]
                  | ExpectElseOpenBraceOrSyntax Expression [Syntax]
                  | ExpectElseCloseBrace Expression [Syntax] [Syntax]

functionAnalyse :: String -> [(Int, Token)] -> Bool -> Int ->
                   Either SyntaxAnalyserError (Int, [Syntax])
functionAnalyse source tokens justOneSyntax = analyse ExpectFirstFactor []
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
                        let newStep = ExpectReturnExpression in
                        analyse newStep contents (index + 1)

                    (_, Keyword "while") ->
                        let newStep = ExpectWhileOpenParentheses in
                        analyse newStep contents (index + 1)

                    (_, Keyword "if") ->
                        let newStep = ExpectIfOpenParentheses in
                        analyse newStep contents (index + 1)

                    (_, Keyword k) | k `elem` typeKeywords ->
                        let newStep = ExpectLocalVariableLabel token in
                        analyse newStep contents (index + 1)

                    (_, Identifier _) ->
                        let newStep = ExpectEqualOrOpenParentheses token in
                        analyse newStep contents (index + 1)

                    (_, CloseBrace) ->
                        Right (index, contents)

                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectReturnExpression ->
                case token of
                    (_, Semicolon) ->
                        let newReturn = Return Void
                            newContents = contents ++ [newReturn]
                            newStep = ExpectFirstFactor in
                        analyse newStep newContents (index + 1)

                    _ ->
                        case expressionAnalyse source tokens index of
                            Right (newIndex, expr) ->
                                let newStep = ExpectReturnSemicolon expr in
                                analyse newStep contents newIndex

                            Left err ->
                                Left $ InvalidExpression err

            (ExpectReturnSemicolon e) ->
                case token of
                    (_, Semicolon) ->
                        let newReturn = Return e
                            newContents = contents ++ [newReturn]
                            newStep = ExpectFirstFactor in
                        analyse newStep newContents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectLocalVariableLabel t) ->
                case token of
                    (_, Identifier _) ->
                        let newStep = ExpectLocalVariableEqualOrSemicolon t token in
                        analyse newStep contents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectLocalVariableEqualOrSemicolon t l) ->
                case token of
                    (_, Symbol '=') ->
                        let newStep = ExpectLocalVariableExpression t l in
                        analyse newStep contents (index + 1)

                    (_, Semicolon) ->
                        let newVariable = VarDefinition t l Nothing
                            newContents = contents ++ [newVariable]
                            newStep = ExpectFirstFactor in
                        analyse newStep newContents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectLocalVariableExpression t l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let newStep = ExpectLocalVariableSemicolon t l expr in
                        analyse newStep contents newIndex
                    Left err ->
                        Left $ InvalidExpression err

            (ExpectLocalVariableSemicolon t l e) ->
                case token of
                    (_, Semicolon) ->
                        let newVariable = VarDefinition t l (Just e)
                            newContents = contents ++ [newVariable]
                            newStep = ExpectFirstFactor in
                        analyse newStep newContents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectEqualOrOpenParentheses l) ->
                case token of
                    (_, Symbol '=') ->
                        let newStep = ExpectExpressionToReassignVariable l in
                        analyse newStep contents (index + 1)

                    (_, OpenParentheses) ->
                        case functionArgAnalyse source tokens (index + 1) of
                            Right (newIndex, args) ->
                                let newStep = ExpectFunctionCallSemicolon l args in
                                analyse newStep contents (newIndex + 1)
                            Left err ->
                                Left err

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectExpressionToReassignVariable l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let newStep = ExpectReassignSemicolon l expr in
                        analyse newStep contents newIndex
                    Left err ->
                        Left $ InvalidExpression err

            (ExpectReassignSemicolon l e) ->
                case token of
                    (_, Semicolon) ->
                        let newReassign = VarReassign l e
                            newContents = contents ++ [newReassign]
                            newStep = ExpectFirstFactor in
                        analyse newStep newContents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectFunctionCallSemicolon l args) ->
                case token of
                    (_, Semicolon) ->
                        let newFuncCall = FunctionCallSyntax l args
                            newContents = contents ++ [newFuncCall]
                            newStep = ExpectFirstFactor in
                        analyse newStep newContents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectWhileOpenParentheses ->
                case token of
                    (_, OpenParentheses) ->
                        let newStep = ExpectWhileCondition in
                        analyse newStep contents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectWhileCondition ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let newStep = ExpectWhileCloseParentheses expr in
                        analyse newStep contents newIndex
                    Left err ->
                        Left $ InvalidExpression err

            (ExpectWhileCloseParentheses cond) ->
                case token of
                    (_, CloseParentheses) ->
                        let newStep = ExpectWhileOpenBraceOrSyntax cond in
                        analyse newStep contents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectWhileOpenBraceOrSyntax cond) ->
                case token of
                    (_, OpenBrace) ->
                        case functionAnalyse source tokens False (index + 1) of
                            Right (newIndex, inner) ->
                                let newStep = ExpectWhileCloseBrace cond inner in
                                analyse newStep contents newIndex
                            Left err ->
                                Left err
                    _ ->
                        case functionAnalyse source tokens True index of
                            Right (newIndex, inner) ->
                                let newWhile = While cond inner
                                    newContents = contents ++ [newWhile]
                                    newStep = ExpectFirstFactor in
                                analyse newStep newContents (newIndex + 1)
                            Left err ->
                                Left err

            (ExpectWhileCloseBrace cond inner) ->
                case token of
                    (_, CloseBrace) ->
                        let newWhile = While cond inner
                            newContents = contents ++ [newWhile]
                            newStep = ExpectFirstFactor in
                        analyse newStep newContents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt
            
            ExpectIfOpenParentheses ->
                case token of
                    (_, OpenParentheses) ->
                        let newStep = ExpectIfCondition in
                        analyse newStep contents (index + 1)
                    
                    _ ->
                        contextualUnexpectedTokenHalt

            ExpectIfCondition ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let newStep = ExpectIfCloseParentheses expr in
                        analyse newStep contents newIndex
                    Left err ->
                        Left $ InvalidExpression err

            (ExpectIfCloseParentheses cond) ->
                case token of
                    (_, CloseParentheses) ->
                        let newStep = ExpectIfOpenBraceOrSyntax cond in
                        analyse newStep contents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectIfOpenBraceOrSyntax cond) ->
                case token of
                    (_, OpenBrace) ->
                        case functionAnalyse source tokens False (index + 1) of
                            Right (newIndex, inner) ->
                                let newStep = ExpectIfCloseBrace cond inner in
                                analyse newStep contents newIndex
                            Left err ->
                                Left err

                    _ ->
                        case functionAnalyse source tokens True index of
                            Right (newIndex, inner) ->
                                let newStep = ExpectElseOrEnd cond inner in
                                analyse newStep contents (newIndex + 1)
                            Left err ->
                                Left err

            (ExpectIfCloseBrace cond inner) ->
                case token of
                    (_, CloseBrace) ->
                        let newStep = ExpectElseOrEnd cond inner in
                        analyse newStep contents (index + 1)
                    
                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectElseOrEnd cond inner) ->
                case token of
                    (_, Keyword "else") ->
                        let newStep = ExpectElseOpenBraceOrSyntax cond inner in
                        analyse newStep contents (index + 1)

                    _ ->
                        let newIf = If cond inner []
                            newContents = contents ++ [newIf]
                            newStep = ExpectFirstFactor in
                        analyse newStep newContents index

            (ExpectElseOpenBraceOrSyntax cond inner) ->
                case token of
                    (_, OpenBrace) ->
                        case functionAnalyse source tokens False (index + 1) of
                            Right (newIndex, elseInner) ->
                                let newStep = ExpectElseCloseBrace cond inner elseInner in
                                analyse newStep contents newIndex
                            Left err ->
                                Left err
                    
                    _ ->
                        case functionAnalyse source tokens True index of
                            Right (newIndex, elseInner) ->
                                let newIf = If cond inner elseInner
                                    newContents = contents ++ [newIf]
                                    newStep = ExpectFirstFactor in
                                analyse newStep newContents (newIndex + 1)
                            Left err ->
                                Left err

            (ExpectElseCloseBrace cond inner elseInner) ->
                case token of
                    (_, CloseBrace) ->
                        let newIf = If cond inner elseInner
                            newContents = contents ++ [newIf]
                            newStep = ExpectFirstFactor in
                        analyse newStep newContents (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

        where
        token = tokens !! index

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
