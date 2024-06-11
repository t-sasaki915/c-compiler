{-# LANGUAGE TemplateHaskell #-}

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

import Control.Lens hiding (index)
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
            -- FunDefinition Type Identifier [ArgumentVariableDef] [Content]
            | FunDefinition (Int, Token) (Int, Token) [Syntax] [Syntax]
            -- VarDefinition Type Identifier (Maybe DefaultValue)
            | VarDefinition (Int, Token) (Int, Token) (Maybe Expression)
            -- Return Value
            | Return Expression
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

data AnalyserStep = ExpectVarOrFunType
                  | ExpectVarOrFunLabel (Int, Token)
                  | ExpectEqualOrOpenParenthesesOrSemicolon (Int, Token) (Int, Token)
                  | ExpectGlobalVariableDefaultValue (Int, Token) (Int, Token)
                  | ExpectGlobalVariableSemicolon (Int, Token) (Int, Token) Expression

data State = State
    { _functions :: [SyntaxTree]
    , _globalVariables :: [SyntaxTree]
    }

makeLenses ''State

syntaxAnalyse :: String -> [(Int, Token)] -> Either SyntaxAnalyserError SyntaxTree
syntaxAnalyse source tokens = analyse ExpectVarOrFunType (State [] []) 0
    where
    analyse :: AnalyserStep -> State -> Int -> Either SyntaxAnalyserError SyntaxTree
    analyse step state index | index >= length tokens =
        case step of
            ExpectVarOrFunType ->
                Right $ SyntaxTree Program (_functions state ++ _globalVariables state)

            _ ->
                Left $ UnexpectedEOF source (fst $ last tokens)

    analyse step state index =
        case step of
            ExpectVarOrFunType ->
                case token of
                    (_, Keyword k) | k `elem` typeKeywords ->
                        let newStep = ExpectVarOrFunLabel token in
                        analyse newStep state (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectVarOrFunLabel t)->
                case token of
                    (_, Identifier _) ->
                        let
                            newStep =
                                ExpectEqualOrOpenParenthesesOrSemicolon t token in
                        analyse newStep state (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt

            (ExpectEqualOrOpenParenthesesOrSemicolon t l) ->
                case token of
                    (_, Semicolon) ->
                        let newStep = ExpectVarOrFunType
                            newVar = SyntaxTree (VarDefinition t l Nothing) []
                            newState = over globalVariables (++ [newVar]) state in
                        analyse newStep newState (index + 1)

                    (_, Symbol '(') ->
                        contextualUnexpectedTokenHalt -- TODO

                    (_, Symbol '=') ->
                        let newStep = ExpectGlobalVariableDefaultValue t l in
                        analyse newStep state (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt
            
            (ExpectGlobalVariableDefaultValue t l) ->
                case expressionAnalyse source tokens index of
                    Right (newIndex, expr) ->
                        let newStep = ExpectGlobalVariableSemicolon t l expr in
                        analyse newStep state newIndex
                    Left err ->
                        Left $ InvalidExpression err

            (ExpectGlobalVariableSemicolon t l d) ->
                case token of
                    (_, Semicolon) ->
                        let newStep = ExpectVarOrFunType
                            newVar = SyntaxTree (VarDefinition t l (Just d)) []
                            newState = over globalVariables (++ [newVar]) state in
                        analyse newStep newState (index + 1)

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
                    (ExpectVarOrFunLabel _) ->
                        "Identifier"
                    (ExpectEqualOrOpenParenthesesOrSemicolon _ _) ->
                        "'=', '(' or ';'"
                    (ExpectGlobalVariableDefaultValue _ _) ->
                        "Expression"
                    (ExpectGlobalVariableSemicolon {}) ->
                        "';'"
