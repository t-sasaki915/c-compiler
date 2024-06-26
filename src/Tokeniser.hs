{-# LANGUAGE TemplateHaskell #-}

module Tokeniser (TokeniserError(..), Token(..), tokenise) where

import ErrorHandling
import ListExtra ((!?))
import TokeniserDomain

import Control.Lens hiding (index)

data TokeniserError = UnexpectedCharacter String Int Char
                    | InvalidNumberFormat String Int String
                    | InvalidToken String Int String
                    deriving (Eq, Show)

instance TrackableError TokeniserError where
    place (UnexpectedCharacter a b _) = (a, b)
    place (InvalidNumberFormat a b _) = (a, b)
    place (InvalidToken a b _)        = (a, b)
    title (UnexpectedCharacter {})    = "Unexpected character"
    title (InvalidNumberFormat {})    = "Invalid number format"
    title (InvalidToken {})           = "Invalid token"
    cause (UnexpectedCharacter _ _ a) = [a]
    cause (InvalidNumberFormat _ _ a) = a
    cause (InvalidToken _ _ a)        = a

data Token = OpenBrace
           | CloseBrace
           | OpenParentheses
           | CloseParentheses
           | Comma
           | Semicolon
           | Equality
           | NotEquality
           | MoreOrEq
           | LessOrEq
           | AndAnd
           | BarBar
           | PlusPlus
           | MinusMinus
           | PlusEqual
           | MinusEqual
           | Symbol Char
           | Keyword String
           | Identifier String
           | Number String
           deriving Eq

instance Show Token where
    show OpenBrace        = "'{'"
    show CloseBrace       = "'}'"
    show OpenParentheses  = "'('"
    show CloseParentheses = "')'"
    show Comma            = "','"
    show Semicolon        = "';'"
    show Equality         = "'=='"
    show NotEquality      = "'!='"
    show MoreOrEq         = "'>='"
    show LessOrEq         = "'<='"
    show AndAnd           = "'&&'"
    show BarBar           = "'||'"
    show PlusPlus         = "'++'"
    show MinusMinus       = "'--'"
    show PlusEqual        = "'+='"
    show MinusEqual       = "'-='"
    show (Symbol a)       = "Symbol '" ++ [a] ++ "'"
    show (Keyword a)      = "Keyword '" ++ a ++ "'"
    show (Identifier a)   = "Identifier '" ++ a ++ "'"
    show (Number a)       = "Number '" ++ a ++ "'" 

data State = State
    { _determined :: [(Int, Token)]
    , _buffer :: String
    , _buffering :: Bool
    , _ignoring :: Bool
    , _ignoringMultiLine :: Bool
    }

makeLenses ''State

tokenise :: String -> Either TokeniserError [(Int, Token)]
tokenise source = analyse (State [] "" False False False) 0
    where
    analyse :: State -> Int -> Either TokeniserError [(Int, Token)]
    analyse state index
        | index >= length source =
            if isBuffering then
                case considerBufferToken of
                    Right token ->
                        Right (_determined state ++ [(index - 1, token)])
                    Left err ->
                        Left err
            else
                Right $ _determined state

        | isIgnoring =
            if chara `elem` acceptableReturns then
                analyse (set ignoring False state) index
            else
                analyseNextCharWith []

        | isIgnoringMultiLine =
            if chara == '*' && source !? (index + 1) == Just '/' then
                analyse (set ignoringMultiLine False state) (index + 2)
            else
                analyseNextCharWith []

        | chara == '/' && source !? (index + 1) == Just '/' =
            analyse (set ignoring True state) index

        | chara == '/' && source !? (index + 1) == Just '*' =
            analyse (set ignoringMultiLine True state) index

        | isBuffering =
            case () of
                () | chara `elem` (acceptableWhitespaces ++ acceptableReturns) ->
                       stopBufferingWith []
                   | chara == '{' ->
                       stopBufferingWith [(index, OpenBrace)]
                   | chara == '}' ->
                       stopBufferingWith [(index, CloseBrace)]
                   | chara == '(' ->
                       stopBufferingWith [(index, OpenParentheses)]
                   | chara == ')' ->
                       stopBufferingWith [(index, CloseParentheses)]
                   | chara == ',' ->
                       stopBufferingWith [(index, Comma)]
                   | chara == ';' ->
                       stopBufferingWith [(index, Semicolon)]
                   | chara == '=' ->
                       case source !? (index + 1) of
                           Just '=' ->
                               stopBufferingWith2 [(index + 1, Equality)]
                           _ ->
                               stopBufferingWith [(index, Symbol chara)]
                   | chara == '!' ->
                       case source !? (index + 1) of
                           Just '=' ->
                               stopBufferingWith2 [(index + 1, NotEquality)]
                           _ ->
                               stopBufferingWith [(index, Symbol chara)]
                   | chara == '>' ->
                       case source !? (index + 1) of
                           Just '=' ->
                               stopBufferingWith2 [(index + 1, MoreOrEq)]
                           _ ->
                               stopBufferingWith [(index, Symbol chara)]
                   | chara == '<' ->
                       case source !? (index + 1) of
                           Just '=' ->
                               stopBufferingWith2 [(index + 1, LessOrEq)]
                           _ ->
                               stopBufferingWith [(index, Symbol chara)]
                   | chara == '&' ->
                       case source !? (index + 1) of
                           Just '&' ->
                               stopBufferingWith2 [(index + 1, AndAnd)]
                           _ ->
                               stopBufferingWith [(index, Symbol chara)]
                   | chara == '|' ->
                       case source !? (index + 1) of
                           Just '|' ->
                               stopBufferingWith2 [(index + 1, BarBar)]
                           _ ->
                               stopBufferingWith [(index, Symbol chara)]
                   | chara == '+' ->
                       case source !? (index + 1) of
                           Just '+' ->
                               stopBufferingWith2 [(index + 1, PlusPlus)]
                           Just '=' ->
                               stopBufferingWith2 [(index + 1, PlusEqual)]
                           _ ->
                               stopBufferingWith [(index, Symbol chara)]
                   | chara == '-' ->
                       case source !? (index + 1) of
                           Just '-' ->
                               stopBufferingWith2 [(index + 1, MinusMinus)]
                           Just '=' ->
                               stopBufferingWith2 [(index + 1, MinusEqual)]
                           _ ->
                               stopBufferingWith [(index, Symbol chara)]
                   | chara `elem` acceptableSymbols ->
                       stopBufferingWith [(index, Symbol chara)]
                   | chara `elem` (acceptableAlphabets ++ acceptableNumbers) ->
                       analyse (over buffer (++ [chara]) state) (index + 1)
                   | otherwise ->
                       Left $ UnexpectedCharacter source index chara

        | otherwise =
            case () of
                () | chara `elem` (acceptableWhitespaces ++ acceptableReturns) ->
                       analyseNextCharWith []
                   | chara == '{' ->
                       analyseNextCharWith [(index, OpenBrace)]
                   | chara == '}' ->
                       analyseNextCharWith [(index, CloseBrace)]
                   | chara == '(' ->
                       analyseNextCharWith [(index, OpenParentheses)]
                   | chara == ')' ->
                       analyseNextCharWith [(index, CloseParentheses)]
                   | chara == ',' ->
                       analyseNextCharWith [(index, Comma)]
                   | chara == ';' ->
                       analyseNextCharWith [(index, Semicolon)]
                   | chara == '=' ->
                       case source !? (index + 1) of
                           Just '=' ->
                               analyseNextCharWith2 [(index + 1, Equality)]
                           _ ->
                               analyseNextCharWith [(index, Symbol chara)]
                   | chara == '!' ->
                       case source !? (index + 1) of
                           Just '=' ->
                               analyseNextCharWith2 [(index + 1, NotEquality)]
                           _ ->
                               analyseNextCharWith [(index, Symbol chara)]
                   | chara == '>' ->
                       case source !? (index + 1) of
                           Just '=' ->
                               analyseNextCharWith2 [(index + 1, MoreOrEq)]
                           _ ->
                               analyseNextCharWith [(index, Symbol chara)]
                   | chara == '<' ->
                       case source !? (index + 1) of
                           Just '=' ->
                               analyseNextCharWith2 [(index + 1, LessOrEq)]
                           _ ->
                               analyseNextCharWith [(index, Symbol chara)]
                   | chara == '&' ->
                       case source !? (index + 1) of
                           Just '&' ->
                               analyseNextCharWith2 [(index + 1, AndAnd)]
                           _ ->
                               analyseNextCharWith [(index, Symbol chara)]
                   | chara == '|' ->
                       case source !? (index + 1) of
                           Just '|' ->
                               analyseNextCharWith2 [(index + 1, BarBar)]
                           _ ->
                               analyseNextCharWith [(index, Symbol chara)]
                   | chara == '+' ->
                       case source !? (index + 1) of
                           Just '+' ->
                               analyseNextCharWith2 [(index + 1, PlusPlus)]
                           Just '=' ->
                               analyseNextCharWith2 [(index + 1, PlusEqual)]
                           _ ->
                               analyseNextCharWith [(index, Symbol chara)]
                   | chara == '-' ->
                       case source !? (index + 1) of
                           Just '-' ->
                               analyseNextCharWith2 [(index + 1, MinusMinus)]
                           Just '=' ->
                               analyseNextCharWith2 [(index + 1, MinusEqual)]
                           _ ->
                               analyseNextCharWith [(index, Symbol chara)]
                   | chara `elem` acceptableSymbols ->
                       analyseNextCharWith [(index, Symbol chara)]
                   | chara `elem` (acceptableAlphabets ++ acceptableNumbers) ->
                       analyse (set buffering True state) index
                   | otherwise ->
                       Left $ UnexpectedCharacter source index chara
        where
        chara = source !! index
        isBuffering = _buffering state
        isIgnoring = _ignoring state
        isIgnoringMultiLine = _ignoringMultiLine state

        considerBufferToken :: Either TokeniserError Token
        considerBufferToken
            | bufferContent `elem` acceptableKeywords = Right $ Keyword bufferContent
            | firstLetter `elem` acceptableAlphabets  = Right $ Identifier bufferContent
            | firstLetter `elem` acceptableNumbers    = Right $ Number bufferContent
            | otherwise = Left $ InvalidToken source (index - 1) bufferContent
            where
            bufferContent = _buffer state
            firstLetter = head bufferContent

        analyseNextCharWith :: [(Int, Token)] -> Either TokeniserError [(Int, Token)]
        analyseNextCharWith newTokens =
            analyse (over determined (++ newTokens) state) (index + 1)

        analyseNextCharWith2 :: [(Int, Token)] -> Either TokeniserError [(Int, Token)]
        analyseNextCharWith2 newTokens =
            analyse (over determined (++ newTokens) state) (index + 2)

        stopBufferingWith :: [(Int, Token)] -> Either TokeniserError [(Int, Token)]
        stopBufferingWith newTokens =
            case considerBufferToken of
                Right token ->
                    analyse (set buffering False
                            (over determined (++ newTokens)
                            (over determined (++ [(index - 1, token)])
                            (set buffer "" state)
                            ))) (index + 1)
                Left err ->
                    Left err

        stopBufferingWith2 :: [(Int, Token)] -> Either TokeniserError [(Int, Token)]
        stopBufferingWith2 newTokens =
            case considerBufferToken of
                Right token ->
                    analyse (set buffering False
                            (over determined (++ newTokens)
                            (over determined (++ [(index - 1, token)])
                            (set buffer "" state)
                            ))) (index + 2)
                Left err ->
                    Left err
