module CommandLineParser (ParsedArgument(..), parseArgs) where

import CommandLine (doesCmdOptionExist, convertAliasToName)

import Data.List (isPrefixOf)

data ParsedArgument = FlagOption String
                    | SourceFileName String
                    | UnrecognisableOption String
                    deriving Show

parseArgs :: [String] -> [ParsedArgument]
parseArgs args = recursively [] 0
    where
    recursively :: [ParsedArgument] -> Int -> [ParsedArgument]
    recursively determined index
        | index >= length args  = determined
        | "-" `isPrefixOf` arg = recursively parseOption (index + 1)
        | otherwise            = recursively parseAppArg (index + 1)
        where
        arg = args !! index

        parseOption =
            let hyphensReduced = if "--" `isPrefixOf` arg then
                                     drop 2 arg
                                 else
                                     drop 1 arg
            in
            if doesCmdOptionExist hyphensReduced then
                determined ++ [FlagOption (convertAliasToName hyphensReduced)]
            else
                determined ++ [UnrecognisableOption hyphensReduced]

        parseAppArg =
            determined ++ [SourceFileName arg]
