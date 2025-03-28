module GridChallenge where

import Data.List (transpose, sort)

ascending :: String -> Bool
ascending "" = True
ascending [_] = True
ascending (c1:c2:cs)
    | c1 <= c2 = ascending (c2:cs)
    | otherwise = False

gridChallenge :: [String] -> String
gridChallenge rs = boolToAnswer
    $ all ascending
    $ transpose 
    $ map sort rs
    where
        boolToAnswer True = "YES"
        boolToAnswer False = "NO"

-- Given an n*n grid, the sorts
-- take O(n^2 log n).
--
-- The transpose step takes O(n^2).
--
-- The ascending check also takes O(n^2).
-- 
-- This makes the whole algorithm O(n^2 log n).
