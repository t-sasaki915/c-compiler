{-# LANGUAGE TemplateHaskell #-}

module CommandLineInterface (startProgram) where

import CommandLine (CmdOption(..), cmdOptions)
import CommandLineParser (ParsedArgument(..))
import Compiler (CompilerError(..), compile)

import Control.Lens hiding (index)
import System.Environment (getProgName)
import System.Exit (ExitCode(..))

printUsage :: IO ()
printUsage = do
    progName <- getProgName
    putStrLn   "Usage:"
    putStrLn $ "    " ++ progName ++ " [OPTIONS] [FILENAME]"
    putStrLn   ""
    putStrLn   "Options:"
    mapM_ printOptionUsage cmdOptions
    putStrLn   ""
    putStrLn   "Arguments:"
    putStrLn   "    FILENAME: Specify where the source file to compile is."
    where
    printOptionUsage :: CmdOption -> IO ()
    printOptionUsage (CmdOption name alias desc) =
        putStrLn $ "    --" ++ name ++ ", -" ++ alias ++ ": " ++ desc

data State = State
    { _shouldShowUsage :: Bool
    , _sourceFileName  :: String
    }

data ArgAnalyseError = UnknownOption String
                     | NotImplementedOption String

makeLenses ''State

startProgram :: [ParsedArgument] -> IO ExitCode
startProgram args =
    case analyseArgs (State False "") 0 of
        Right (State True _) ->
            printUsage >>
                return ExitSuccess

        Right (State False "") -> do
            putStrLn "Invalid usage of this program."
            printUsage
            return (ExitFailure 2)

        Right (State False fileName) -> do
            result <- compile fileName
            case result of
                Right () ->
                    return ExitSuccess

                Left (IOFailure stage msg) ->
                    putStrLn ("An IOError has occurred during " ++ stage ++ ": " ++ msg) >>
                        return (ExitFailure 1)

                Left (TokeniserFailure tokeniserErr) -> do
                    putStrLn "The tokenisation process has aborted!!"
                    print tokeniserErr
                    return (ExitFailure 1)

                Left (SyntaxAnalyserFailure syntaxAnalyserErr) -> do
                    putStrLn "The syntax analysation process has aborted!!"
                    print syntaxAnalyserErr
                    return (ExitFailure 1)

        Left (UnknownOption str) -> do
            putStrLn $ "There was an unrecognisable option reference: " ++ str
            printUsage
            return (ExitFailure 2)

        Left (NotImplementedOption str) -> do
            putStrLn $ "This feature currently has no implementation: " ++ str
            printUsage
            return (ExitFailure 1)
    where
    analyseArgs :: State -> Int -> Either ArgAnalyseError State
    analyseArgs state index
        | index >= length args = Right state
        | otherwise =
            case args !! index of
                FlagOption "help" ->
                    analyseArgs (set shouldShowUsage True state) (index + 1)

                SourceFileName str ->
                    analyseArgs (set sourceFileName str state) (index + 1)

                UnrecognisableOption str ->
                    Left $ UnknownOption str

                FlagOption str ->
                    Left $ NotImplementedOption str
