module Main (main) where

import CommandLineInterface (startProgram)
import CommandLineParser (parseArgs)

import System.Environment (getArgs)
import System.Exit (exitWith)

main :: IO ()
main =
    getArgs >>= (startProgram . parseArgs) >>= exitWith
