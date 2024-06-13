module Compiler (CompilerError(..), compile) where

import SemanticVerifier (SemanticError(..), semanticVerify)
import SyntaxAnalyser (SyntaxAnalyserError(..), syntaxAnalyse)
import Tokeniser (TokeniserError(..), tokenise)

import Control.Exception (try)
import System.IO.Error (ioeGetErrorString)

data CompilerError = IOFailure String String
                   | TokeniserFailure TokeniserError
                   | SyntaxAnalyserFailure SyntaxAnalyserError
                   | SemanticVerifierFailure SemanticError

class ToCompilerError a where
    wrapErr :: a -> CompilerError

instance ToCompilerError TokeniserError where
    wrapErr = TokeniserFailure
instance ToCompilerError SyntaxAnalyserError where
    wrapErr = SyntaxAnalyserFailure
instance ToCompilerError SemanticError where
    wrapErr = SemanticVerifierFailure

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f = either (Left . f) Right

compiling :: String -> Either CompilerError String
compiling src =
    mapLeft wrapErr (tokenise src) >>=
        (mapLeft wrapErr . syntaxAnalyse src) >>=
            (mapLeft wrapErr . semanticVerify src) >>=
                const (Right "")

compile :: String -> IO (Either CompilerError ())
compile sourceFileName = do
    cSourceOrErr <- try $ readFile sourceFileName :: IO (Either IOError String)
    case cSourceOrErr of
        Right cSource -> do
            case compiling cSource of
                Right _ ->
                    return $ Right ()
                
                Left err ->
                    return $ Left err

        Left readErr ->
            return $ Left (IOFailure "READ" (ioeGetErrorString readErr))
