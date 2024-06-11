module ListExtra ((!?)) where

(!?) :: [a] -> Int -> Maybe a
(!?) xs n
    | n > length xs - 1 = Nothing
    | otherwise         = Just $ xs !! n
