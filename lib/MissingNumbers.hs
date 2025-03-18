module MissingNumbers where

-- We can compute the difference
-- of two sets in O(m * log n)
-- time where m is the length
-- of the shortest set.
--
-- In other words, for each
-- element in the small set,
-- we perform a binary search
-- on the second set.
--
-- Unfortunately duplicate elements
-- complicates this slightly.
-- A custom version of a BST
-- which stores the count of each
-- element would help, however,
-- this would mean implementing
-- a balanced BST from scratch
-- which is not ideal.

-- Maps provide a BST-like
-- implementation which should
-- allow us to store the count.

-- Unfortunately the cost of
-- creating the map would be
-- approximately O(m * log m).
--
-- Added to the difference
-- computation we get O( n logm + m logm )
-- ~ O((n + m) log m)
-- which is hardly better than
-- sorting both lists and iterating
-- over them at O( n logn + m logm + n)...
--
-- If we store the counts of elements
-- in the longer array in a HashMap,
-- then we get O(1) lookups.
--
-- So our total time becomes
-- O( m log m + n). But that would
-- degrade back to the previous
-- complexity if we had to update
-- the count for every lookup.
--
-- Since the difference between the
-- min and max values is <=100, we
-- can map every entry to a number from
-- 0 - 99 where we then index a vector
-- of counts.
--
-- Constructing such a vector would cost
-- O(m + n) since we would want to find
-- the min value first.
--
-- We would then iterate over the larger
-- array and modify the counts in the vector,
-- if the count for an index is 0 then
-- we can add that element to our "missing list".
--
-- Our missing list could be stored as a Set to
-- remove duplicates. But this would require O(log n)
-- for each insert.
--
-- It would probably be better, or at least more consistent
-- to decrement the count in the vector, then count
-- any negative counts.

import Control.Monad (forM_)
import Control.Monad.ST
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV

mkCounts :: Int -> [Int] -> Vector Int
mkCounts offset xs = runST $ do
    mv <- MV.replicate 101 0
    forM_ xs $ \x -> MV.modify mv (+ 1) (x - offset)
    V.freeze mv

checkMembers :: Int -> Vector Int -> [Int] -> Vector Int
checkMembers offset counts xs0 = runST $ do
    mv <- V.thaw counts
    go mv xs0
    where
        go mcounts [] = V.freeze mcounts
        go mcounts (x : xs) = do
            let ix = x - offset
            MV.modify mcounts (\n -> n - 1) ix
            go mcounts xs

missingNumbers :: [Int] -> [Int] -> [Int]
missingNumbers xs ys =
    [ i + mn
    | (c, i) <- zip (V.toList finalCounts) [0 ..]
    , c < 0
    ]
    where
        finalCounts = checkMembers mn initCounts ys
        initCounts = mkCounts mn xs
        mn = minimum ys
