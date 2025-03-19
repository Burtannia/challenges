module LonelyInt where

import Control.Monad (filterM)
import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.List (foldl1')
import Data.Bits (xor)

-- There are a few way to approach this,
-- all providing O(n) solutions.
--
-- Firstly we can insert the values
-- into a HashMap, check for collisions
-- and then we'll have the result.
-- This is expected time O(n), but
-- has a theoretical worst case of
-- O(n log n).
--
-- Since we only have up to 100 numbers,
-- we can use a vector instead to guarantee
-- the O(n) complexity.
--
-- The numbers are between 0 and 100 so
-- there is also no need for hashing to
-- an index, we just use the number itself.
--
-- I'm wondering if the other constraint,
-- "guaranteed that n is odd" gives rise
-- to a better than O(n) solution, potentially,
-- in some sort of QuickSort splitting style.
--
-- This would work, but I don't think it
-- would get us below O(n).

vectorApproach :: [Int] -> Int
vectorApproach xs = runST $ do
    mv <- MV.replicate 101 0
    go mv xs
    where
        go :: forall s. STVector s Int -> [Int] -> ST s Int
        go mv [] = do
            let ixs = [0 .. 100] :: [Int]
            uniques <- filterM (\i -> (== 1) <$> MV.read mv i) ixs
            pure $ head uniques
        go mv (y : ys) = do
            MV.modify mv (+ 1) y
            go mv ys

-- It seems we can use xor to achieve the same
-- O(n) time complexity, but with a much simpler
-- implementation, and O(1) space complexity.
--
-- The clue here was that the non-unique numbers
-- are in pairs.

xorApproach :: [Int] -> Int
xorApproach = foldl1' xor
