module SyntaxAnalyser
    ( SyntaxAnalyserError(..)
    , Syntax(..)
    , syntaxAnalyse
    ) where

import ErrorHandling
import ExpressionAnalyser (Expression(..), ExpressionAnalyserError, expressionAnalyse)
import Tokeniser (Token(..))
import TokeniserDomain (typeKeywords)

data SyntaxAnalyserError = UnexpectedToken String Int Token String
                         | UnexpectedEOF String Int
                         | InvalidExpression ExpressionAnalyserError
                         deriving (Eq, Show)

instance TrackableError SyntaxAnalyserError where
    place (UnexpectedToken a b _ _) = (a, b)
    place (UnexpectedEOF a b)       = (a, b)
    place (InvalidExpression a)     = place a
    title (UnexpectedToken {})      = "Unexpected token"
    title (UnexpectedEOF {})        = "Unexpected end of file"
    title (InvalidExpression a)     = title a
    cause (UnexpectedToken _ _ a b) =
        "Expected " ++ b ++ " but " ++ show a ++ " has found."
    cause (UnexpectedEOF {})        = ""
    cause (InvalidExpression a)     = cause a

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
            | For (Maybe Syntax) (Maybe Expression) (Maybe Syntax) [Syntax]
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
                        unexpectedTokenHalt "Type"

            (ExpectVarOrFunLabel t)->
                case token of
                    (_, Identifier _) ->
                        nextStep $ ExpectEqualOrOpenParenthesesOrSemicolon t token

                    _ ->
                        unexpectedTokenHalt "Identifier"

            (ExpectEqualOrOpenParenthesesOrSemicolon t l) ->
                case token of
                    (_, Semicolon) ->
                        determine $ VarDefinition t l Nothing

                    (_, OpenParentheses) ->
                        nextStep $ ExpectFunArgTypeOrEnd t l

                    (_, Symbol '=') ->
                        nextStep $ ExpectGlobalVariableDefaultValue t l

                    _ ->
                        unexpectedTokenHalt "';', '(' or '='"

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
                        unexpectedTokenHalt "';'"

            (ExpectFunArgTypeOrEnd t l) ->
                case token of
                    (_, Keyword "void") ->
                        nextStep $ ExpectFunArgEnd t l

                    (_, Keyword k) | k `elem` typeKeywords ->
                        nextStep $ ExpectFunArgLabel t l token []

                    (_, CloseParentheses) ->
                        nextStep $ ExpectFunOpenBrace t l []

                    _ ->
                        unexpectedTokenHalt "Type or ')'"

            (ExpectFunArgEnd t l) ->
                case token of
                    (_, CloseParentheses) ->
                        nextStep $ ExpectFunOpenBrace t l []

                    _ ->
                        unexpectedTokenHalt "')'"

            (ExpectFunArgType t l determined) ->
                case token of
                    (_, Keyword k) | k `elem` typeKeywords ->
                        nextStep $ ExpectFunArgLabel t l token determined

                    _ ->
                        unexpectedTokenHalt "Type"

            (ExpectFunArgLabel t l at determined) ->
                case token of
                    (_, Identifier _) ->
                        nextStep $ ExpectFunArgCommaOrEnd t l at token determined

                    _ ->
                        unexpectedTokenHalt "Identifier"

            (ExpectFunArgCommaOrEnd t l at al determined) ->
                case token of
                    (_, Comma) ->
                        let newArg = VarDefinition at al Nothing in
                        nextStep $ ExpectFunArgType t l (determined ++ [newArg])

                    (_, CloseParentheses) ->
                        let newArg = VarDefinition at al Nothing in
                        nextStep $ ExpectFunOpenBrace t l (determined ++ [newArg])

                    _ ->
                        unexpectedTokenHalt "',' or ')'"

            (ExpectFunOpenBrace t l args) ->
                case token of
                    (_, OpenBrace) ->
                        case functionAnalyse source tokens False False (index + 1) of
                            Right (newIndex, contents) ->
                                nextStep' (ExpectFunCloseBrace t l args contents) newIndex
                                
                            Left err ->
                                Left err

                    _ ->
                        unexpectedTokenHalt "'{'"

            (ExpectFunCloseBrace t l args contents) ->
                case token of
                    (_, CloseBrace) ->
                        determine $ FunDefinition t l args contents

                    _ ->
                        unexpectedTokenHalt "'}'"
        where
        token = tokens !! index

        nextStep' newStep = analyse newStep defs
        nextStep newStep = nextStep' newStep (index + 1)
        determine' newSyntax = analyse ExpectVarOrFunType (defs ++ [newSyntax])
        determine newSyntax = determine' newSyntax (index + 1)
        unexpectedTokenHalt e = Left $ uncurry (UnexpectedToken source) token e

type MSyntax = Maybe Syntax

data FAnalyseStep = ExpectFirstFactor
                  | ExpectReturnExpression
                  | ExpectReturnSemicolon Expr
                  | ExpectLocalVariableLabel IToken
                  | ExpectLocalVariableEqualOrSemicolon IToken IToken
                  | ExpectLocalVariableExpression IToken IToken
                  | ExpectLocalVariableSemicolon IToken IToken Expr
                  | ExpectEqualOrOpenParentheses IToken
                  | ExpectVariableToPlusPlusReassign
                  | ExpectVariableToMinusMinusReassign
                  | ExpectExpressionToReassignVariable IToken
                  | ExpectExpressionToReassignVariablePlusEq IToken
                  | ExpectExpressionToReassignVariableMinusEq IToken
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
                  | ExpectForOpenParentheses
                  | ExpectForFirstAssignOrSemicolon
                  | ExpectForFirstSemicolon MSyntax
                  | ExpectForConditionOrSemicolon MSyntax
                  | ExpectForSecondSemicolon MSyntax (Maybe Expression)
                  | ExpectForSecondAssignOrEnd MSyntax (Maybe Expression)
                  | ExpectForCloseParentheses MSyntax (Maybe Expression) MSyntax
                  | ExpectForOpenBraceOrSyntax MSyntax (Maybe Expression) MSyntax
                  | ExpectForCloseBrace MSyntax (Maybe Expression) MSyntax [Syntax]

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

                    (_, Keyword "for") ->
                        nextStep ExpectForOpenParentheses

                    (_, Keyword "continue") | insideLoop ->
                        nextStep ExpectContinueSemicolon

                    (_, Keyword "break") | insideLoop ->
                        nextStep ExpectBreakSemicolon

                    (_, Keyword k) | k `elem` typeKeywords ->
                        nextStep $ ExpectLocalVariableLabel token

                    (_, Identifier _) ->
                        nextStep $ ExpectEqualOrOpenParentheses token

                    (_, PlusPlus) ->
                        nextStep ExpectVariableToPlusPlusReassign

                    (_, MinusMinus) ->
                        nextStep ExpectVariableToMinusMinusReassign

                    (_, CloseBrace) ->
                        Right (index, contents)

                    _ ->
                        unexpectedTokenHalt "Keyword, Identifier or '}'"

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
                        unexpectedTokenHalt "';'"

            (ExpectLocalVariableLabel t) ->
                case token of
                    (_, Identifier _) ->
                        nextStep $ ExpectLocalVariableEqualOrSemicolon t token

                    _ ->
                        unexpectedTokenHalt "Identifier"

            (ExpectLocalVariableEqualOrSemicolon t l) ->
                case token of
                    (_, Symbol '=') ->
                        nextStep $ ExpectLocalVariableExpression t l

                    (_, Semicolon) ->
                        determine $ VarDefinition t l Nothing

                    _ ->
                        unexpectedTokenHalt "'=' or ';'"

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
                        unexpectedTokenHalt "';'"

            (ExpectEqualOrOpenParentheses l) ->
                case token of
                    (_, Symbol '=') ->
                        nextStep $ ExpectExpressionToReassignVariable l

                    (_, PlusEqual) ->
                        nextStep $ ExpectExpressionToReassignVariablePlusEq l
                    
                    (_, MinusEqual) ->
                        nextStep $ ExpectExpressionToReassignVariableMinusEq l

                    (n, PlusPlus) ->
                        let calc = Addition (VarReference l)
                                            (NumReference (n, Number "1")) in
                        nextStep $ ExpectReassignSemicolon l calc

                    (n, MinusMinus) ->
                        let calc = Subtraction (VarReference l)
                                               (NumReference (n, Number "1")) in
                        nextStep $ ExpectReassignSemicolon l calc

                    (_, OpenParentheses) ->
                        case functionArgAnalyse source tokens (index + 1) of
                            Right (newIndex, args) ->
                                nextStep'
                                    (ExpectFunctionCallSemicolon l args) (newIndex + 1)

                            Left err ->
                                Left err

                    _ ->
                        unexpectedTokenHalt "'=' or '('"

            ExpectVariableToPlusPlusReassign ->
                case token of
                    (_, Identifier _) ->
                        let symbolIndex = fst (tokens !! subtract 1 index)
                            calc = Addition (VarReference token)
                                            (NumReference (symbolIndex, Number "1")) in
                        nextStep $ ExpectReassignSemicolon token calc

                    _ ->
                        unexpectedTokenHalt "Identifier"

            ExpectVariableToMinusMinusReassign ->
                case token of
                    (_, Identifier _) ->
                        let symbolIndex = fst (tokens !! subtract 1 index)
                            calc = Subtraction (VarReference token)
                                               (NumReference (symbolIndex, Number "1")) in
                        nextStep $ ExpectReassignSemicolon token calc

                    _ ->
                        unexpectedTokenHalt "Identifier"

            (ExpectExpressionToReassignVariable l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        nextStep' (ExpectReassignSemicolon l expr) newIndex

                    Left err ->
                        Left $ InvalidExpression err
            
            (ExpectExpressionToReassignVariablePlusEq l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let calc = Addition (VarReference l) expr in
                        nextStep' (ExpectReassignSemicolon l calc) newIndex

                    Left err ->
                        Left $ InvalidExpression err

            (ExpectExpressionToReassignVariableMinusEq l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let calc = Subtraction (VarReference l) expr in
                        nextStep' (ExpectReassignSemicolon l calc) newIndex

                    Left err ->
                        Left $ InvalidExpression err

            (ExpectReassignSemicolon l e) ->
                case token of
                    (_, Semicolon) ->
                        determine $ VarReassign l e

                    _ ->
                        unexpectedTokenHalt "';'"

            (ExpectFunctionCallSemicolon l args) ->
                case token of
                    (_, Semicolon) ->
                        determine $ FunctionCallSyntax l args

                    _ ->
                        unexpectedTokenHalt "';'"

            ExpectWhileOpenParentheses ->
                case token of
                    (_, OpenParentheses) ->
                        nextStep ExpectWhileCondition

                    _ ->
                        unexpectedTokenHalt "'('"

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
                        unexpectedTokenHalt "')'"

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
                        unexpectedTokenHalt "'}'"

            ExpectContinueSemicolon ->
                case token of
                    (_, Semicolon) ->
                        determine Continue
                    
                    _ -> 
                        unexpectedTokenHalt "';'"

            ExpectBreakSemicolon ->
                case token of
                    (_, Semicolon) ->
                        determine Break

                    _ ->
                        unexpectedTokenHalt "';'"
            
            ExpectIfOpenParentheses ->
                case token of
                    (_, OpenParentheses) ->
                        nextStep ExpectIfCondition
                    
                    _ ->
                        unexpectedTokenHalt "'('"

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
                        unexpectedTokenHalt "')'"

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
                        unexpectedTokenHalt "'}'"

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
                        unexpectedTokenHalt "'}'"

            ExpectForOpenParentheses ->
                case token of
                    (_, OpenParentheses) ->
                        nextStep ExpectForFirstAssignOrSemicolon

                    _ ->
                        unexpectedTokenHalt "'('"

            ExpectForFirstAssignOrSemicolon ->
                case token of
                    (_, Semicolon) ->
                        nextStep $ ExpectForConditionOrSemicolon Nothing

                    _ ->
                        case assignAnalyse source tokens index of
                            Right (newIndex, assign) ->
                                nextStep'
                                    (ExpectForFirstSemicolon (Just assign)) newIndex

                            Left err ->
                                Left err

            (ExpectForFirstSemicolon fAssign) ->
                case token of
                    (_, Semicolon) ->
                        nextStep $ ExpectForConditionOrSemicolon fAssign

                    _ ->
                        unexpectedTokenHalt "';'"

            (ExpectForConditionOrSemicolon fAssign) ->
                case token of
                    (_, Semicolon) ->
                        nextStep $ ExpectForSecondAssignOrEnd fAssign Nothing

                    _ ->
                        case expressionAnalyse source tokens index of
                            Right (newIndex, expr) ->
                                nextStep' 
                                    (ExpectForSecondSemicolon fAssign (Just expr))
                                        newIndex

                            Left err ->
                                Left $ InvalidExpression err

            (ExpectForSecondSemicolon fAssign cond) ->
                case token of
                    (_, Semicolon) ->
                        nextStep $ ExpectForSecondAssignOrEnd fAssign cond

                    _ ->
                        unexpectedTokenHalt "';'"

            (ExpectForSecondAssignOrEnd fAssign cond) ->
                case token of
                    (_, CloseParentheses) ->
                        nextStep $ ExpectForOpenBraceOrSyntax fAssign cond Nothing

                    _ ->
                        case assignAnalyse source tokens index of
                            Right (newIndex, assign) ->
                                nextStep'
                                    (ExpectForCloseParentheses fAssign cond (Just assign))
                                            newIndex

                            Left err ->
                                Left err

            (ExpectForCloseParentheses fAssign cond sAssign) ->
                case token of
                    (_, CloseParentheses) ->
                        nextStep $ ExpectForOpenBraceOrSyntax fAssign cond sAssign

                    _ ->
                        unexpectedTokenHalt "')'"

            (ExpectForOpenBraceOrSyntax fAssign cond sAssign) ->
                case token of
                    (_, OpenBrace) ->
                        case functionAnalyse source tokens False True (index + 1) of
                            Right (newIndex, inner) ->
                                nextStep'
                                    (ExpectForCloseBrace fAssign cond sAssign inner)
                                        newIndex

                            Left err ->
                                Left err

                    _ ->
                        case functionAnalyse source tokens True True index of
                            Right (newIndex, inner) ->
                                determine'
                                    (For fAssign cond sAssign inner) (newIndex + 1) 

                            Left err ->
                                Left err

            (ExpectForCloseBrace fAssign cond sAssign inner) ->
                case token of
                    (_, CloseBrace) ->
                        determine $ For fAssign cond sAssign inner

                    _ ->
                        unexpectedTokenHalt "'}'"

        where
        token = tokens !! index

        nextStep' newStep = analyse newStep contents
        nextStep newStep = nextStep' newStep (index + 1)
        determine' newContent = analyse ExpectFirstFactor (contents ++ [newContent])
        determine newContent = determine' newContent (index + 1)
        unexpectedTokenHalt e = Left $ uncurry (UnexpectedToken source) token e

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

                    (a, b) ->
                        Left $ UnexpectedToken source a b "',' or ')'"

            ExpectArgument ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        analyse ExpectCommaOrEnd (args ++ [expr]) newIndex

                    Left err ->
                        Left $ InvalidExpression err

        where
        token = tokens !! index

data AssignAnalyseStep = ExpectKeywordOrIdentifier
                       | ExpectVarToPlusPlus
                       | ExpectVarToMinusMinus
                       | ExpectNewVarLabel (Int, Token)
                       | ExpectNewVarEqualOrEnd (Int, Token) (Int, Token)
                       | ExpectNewVarValue (Int, Token) (Int, Token)
                       | ExpectNewVarEnd (Int, Token) (Int, Token) Expression
                       | ExpectReassignEqual (Int, Token)
                       | ExpectReassignValue (Int, Token)
                       | ExpectReassignValueToPlusEq (Int, Token)
                       | ExpectReassignValueToMinusEq (Int, Token)
                       | ExpectReassignEnd (Int, Token) Expression

assignAnalyse :: String -> [(Int, Token)] -> Int ->
                 Either SyntaxAnalyserError (Int, Syntax)
assignAnalyse source tokens = analyse ExpectKeywordOrIdentifier
    where
    analyse :: AssignAnalyseStep -> Int -> Either SyntaxAnalyserError (Int, Syntax)
    analyse _ index | index >= length tokens =
        Left $ UnexpectedEOF source (fst $ last tokens)

    analyse step index =
        case step of
            ExpectKeywordOrIdentifier ->
                case token of
                    (_, Keyword k) | k `elem` typeKeywords ->
                        nextStep $ ExpectNewVarLabel token

                    (_, Identifier _) ->
                        nextStep $ ExpectReassignEqual token

                    (_, PlusPlus) ->
                        nextStep ExpectVarToPlusPlus

                    (_, MinusMinus) ->
                        nextStep ExpectVarToMinusMinus

                    _ ->
                        unexpectedTokenHalt "Type or Identifier"

            ExpectVarToPlusPlus ->
                case token of
                    (_, Identifier _) ->
                        let symbolIndex = fst (tokens !! subtract 1 index)
                            calc = Addition (VarReference token)
                                            (NumReference (symbolIndex, Number "1")) in
                        nextStep $ ExpectReassignEnd token calc

                    _ ->
                        unexpectedTokenHalt "Identifier"

            ExpectVarToMinusMinus ->
                case token of
                    (_, Identifier _) ->
                        let symbolIndex = fst (tokens !! subtract 1 index)
                            calc = Subtraction (VarReference token)
                                               (NumReference (symbolIndex, Number "1")) in
                        nextStep $ ExpectReassignEnd token calc

                    _ ->
                        unexpectedTokenHalt "Identifier"

            (ExpectNewVarLabel t) ->
                case token of
                    (_, Identifier _) ->
                        nextStep $ ExpectNewVarEqualOrEnd t token

                    _ ->
                        unexpectedTokenHalt "Identifier"

            (ExpectNewVarEqualOrEnd t l) ->
                case token of
                    (_, Symbol '=') ->
                        nextStep $ ExpectNewVarValue t l

                    (_, Semicolon) ->
                        Right (index, VarDefinition t l Nothing)

                    (_, CloseParentheses) ->
                        Right (index, VarDefinition t l Nothing)

                    _ ->
                        unexpectedTokenHalt "'=', ';' or ')'"

            (ExpectNewVarValue t l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        nextStep' (ExpectNewVarEnd t l expr) newIndex

                    Left err ->
                        Left $ InvalidExpression err

            (ExpectNewVarEnd t l v) ->
                case token of
                    (_, Semicolon) ->
                        Right (index, VarDefinition t l (Just v))

                    (_, CloseParentheses) ->
                        Right (index, VarDefinition t l (Just v))

                    _ ->
                        unexpectedTokenHalt "';' or ')'"
            
            (ExpectReassignEqual l) ->
                case token of
                    (_, Symbol '=') ->
                        nextStep $ ExpectReassignValue l

                    (_, PlusEqual) ->
                        nextStep $ ExpectReassignValueToPlusEq l

                    (_, MinusEqual) ->
                        nextStep $ ExpectReassignValueToMinusEq l

                    (n, PlusPlus) ->
                        let calc = Addition (VarReference l)
                                            (NumReference (n, Number "1")) in
                        nextStep $ ExpectReassignEnd l calc

                    (n, MinusMinus) ->
                        let calc = Subtraction (VarReference l)
                                               (NumReference (n, Number "1")) in
                        nextStep $ ExpectReassignEnd l calc

                    _ ->
                        unexpectedTokenHalt "'='"

            (ExpectReassignValue l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        nextStep' (ExpectReassignEnd l expr) newIndex

                    Left err ->
                        Left $ InvalidExpression err

            (ExpectReassignValueToPlusEq l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let calc = Addition (VarReference l) expr in
                        nextStep' (ExpectReassignEnd l calc) newIndex

                    Left err ->
                        Left $ InvalidExpression err
            
            (ExpectReassignValueToMinusEq l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let calc = Subtraction (VarReference l) expr in
                        nextStep' (ExpectReassignEnd l calc) newIndex

                    Left err ->
                        Left $ InvalidExpression err
            
            (ExpectReassignEnd l v) ->
                case token of
                    (_, Semicolon) ->
                        Right (index, VarReassign l v)

                    (_, CloseParentheses) ->
                        Right (index, VarReassign l v)

                    _ ->
                        unexpectedTokenHalt "';' or ')'"
        where
        token = tokens !! index

        nextStep' = analyse
        nextStep newStep = nextStep' newStep (index + 1)
        unexpectedTokenHalt e = Left $ uncurry (UnexpectedToken source) token e
