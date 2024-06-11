module SourceFileAnalyser
    ( getLineNumber
    , getIndexOfLine
    , sourceLoc
    ) where

import Data.List (findIndices)

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

lastIndex :: (a -> Bool) -> [a] -> Maybe Int
lastIndex f xs =
    case findIndices f xs of
        [] -> Nothing
        ys -> Just $ last ys

getLineNumber :: String -> Int -> Int
getLineNumber source index =
    1 + count ('\n' ==) targetPart
    where
    targetPart = take (index + 1) source

getIndexOfLine :: String -> Int -> Int
getIndexOfLine source index =
    case lastIndex ('\n' ==) targetPart of
        Just x -> index - x - 1
        Nothing -> index
    where
    targetPart = take (index + 1) source

sourceLoc :: String -> Int -> String
sourceLoc source index = "Line " ++ show line ++ ", Index " ++ show index'
    where
    line = getLineNumber source index
    index' = getIndexOfLine source index
