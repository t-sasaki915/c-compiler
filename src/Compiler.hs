module Compiler (CompilerError(..), compile) where

import SyntaxAnalyser (SyntaxAnalyserError(..), syntaxAnalyse)
import Tokeniser (TokeniserError(..), tokenise)

import Control.Exception (try)
import System.IO.Error (ioeGetErrorString)

data CompilerError = IOFailure String String
                   | TokeniserFailure TokeniserError
                   | SyntaxAnalyserFailure SyntaxAnalyserError

compile :: String -> IO (Either CompilerError ())
compile sourceFileName = do
    cSourceOrErr <- try $ readFile sourceFileName :: IO (Either IOError String)
    case cSourceOrErr of
        Right cSource -> do
            case tokenise cSource of
                Right tokens ->
                    case syntaxAnalyse cSource tokens of
                        Right structure -> do
                            print structure
                            return $ Right ()

                        Left sErr ->
                            return $ Left (SyntaxAnalyserFailure sErr)

                Left tErr ->
                    return $ Left (TokeniserFailure tErr)

        Left readErr ->
            return $ Left (IOFailure "READ" (ioeGetErrorString readErr))
