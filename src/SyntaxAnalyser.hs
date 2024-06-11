{-# LANGUAGE TemplateHaskell #-}

module SyntaxAnalyser
  ( SyntaxAnalyserError(..)
  , Syntax(..)
  , SyntaxTree(..)
  , syntaxAnalyse
  ) where

import ExpressionAnalyser (Expression(..), ExpressionAnalyserError)
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

data AnalyserStep = WaitingForVarOrFunType
                  | WaitingForVarOrFunLabel (Int, Token)
                  | WaitingForEqualOrOpenParenthesesOrSemicolon (Int, Token) (Int, Token)

data State = State
    { _functions :: [SyntaxTree]
    , _globalVariables :: [SyntaxTree]
    }

makeLenses ''State

syntaxAnalyse :: String -> [(Int, Token)] -> Either SyntaxAnalyserError SyntaxTree
syntaxAnalyse source tokens = analyse WaitingForVarOrFunType (State [] []) 0
    where
    analyse :: AnalyserStep -> State -> Int -> Either SyntaxAnalyserError SyntaxTree
    analyse step state index | index >= length tokens =
        case step of
            WaitingForVarOrFunType ->
                Right $ SyntaxTree Program (_functions state ++ _globalVariables state)
        
            _ ->
                Left $ UnexpectedEOF source (fst $ last tokens)

    analyse step state index =
        case step of
            WaitingForVarOrFunType ->
                case token of
                    (_, Keyword k) | k `elem` typeKeywords ->
                        let newStep = WaitingForVarOrFunLabel token in
                        analyse newStep state (index + 1)

                    _ ->
                        contextualUnexpectedTokenHalt
            
            (WaitingForVarOrFunLabel t)->
                case token of
                    (_, Identifier _) ->
                        let 
                            newStep =
                                WaitingForEqualOrOpenParenthesesOrSemicolon t token in
                        analyse newStep state (index + 1)
                    
                    _ ->
                        contextualUnexpectedTokenHalt
            
            (WaitingForEqualOrOpenParenthesesOrSemicolon t l) ->
                case token of
                    (_, Semicolon) ->
                        let newStep = WaitingForVarOrFunType
                            newVariable = SyntaxTree (VarDefinition t l Nothing) []
                            newState = over globalVariables (++ [newVariable]) state in
                        analyse newStep newState (index + 1)

                    (_, Symbol '(') ->
                        contextualUnexpectedTokenHalt -- TODO
                    
                    (_, Symbol '=') ->
                        contextualUnexpectedTokenHalt -- TODO
                    
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
                    WaitingForVarOrFunType ->
                        "Type"
                    (WaitingForVarOrFunLabel _) ->
                        "Identifier"
                    (WaitingForEqualOrOpenParenthesesOrSemicolon _ _) ->
                        "'=', '(' or ';'"
