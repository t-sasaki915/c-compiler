module CommandLine
  ( CmdOption(..)
  , cmdOptions
  , doesCmdOptionExist
  , convertAliasToName
  ) where

data CmdOption = CmdOption String String String deriving Show

cmdOptions :: [CmdOption]
cmdOptions =
    [ CmdOption "help" "h" "Print how to use this program and exit."
    ]

nameOf :: CmdOption -> String
nameOf (CmdOption name _ _) = name

aliasOf :: CmdOption -> String
aliasOf (CmdOption _ alias _) = alias

doesCmdOptionExist :: String -> Bool
doesCmdOptionExist nameOrAlias = byName || byAlias
    where
    byName = nameOrAlias `elem` map nameOf cmdOptions
    byAlias = nameOrAlias `elem` map aliasOf cmdOptions

getCmdOption :: String -> CmdOption
getCmdOption nameOrAlias = head (byName ++ byAlias)
    where
    byName = filter (\opt -> nameOrAlias == nameOf opt) cmdOptions
    byAlias = filter (\opt -> nameOrAlias == aliasOf opt) cmdOptions

convertAliasToName :: String -> String
convertAliasToName nameOrAlias =
    case getCmdOption nameOrAlias of
        (CmdOption name _ _) -> name
