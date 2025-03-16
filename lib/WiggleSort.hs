{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module WiggleSort where

import Control.Monad.State
import Data.List (partition, sort)
import System.Random

-- a simple solution involves sorting the list
-- splitting it in two, to get a list of "lowers"
-- and a list of "uppers", zipping the two together
-- then flattening the zipped list...
--
-- We have to be careful with the zip since if either
-- list has 1 more element, it will be lost.
-- We can resolve this by taking the first element
-- of lowers, ensuring lowers is always the longest
-- and adding it to the end of the solution.

flatten :: [(a, a)] -> [a]
flatten [] = []
flatten ((x, y) : xs) = x : y : flatten xs

-- If uneven, the first element is returned as the extra
splitEvenly :: [a] -> ([a], [a], Maybe a)
splitEvenly [] = ([], [], Nothing)
splitEvenly xs@(y : ys) = go (length xs `divMod` 2)
    where
        go (half, 0) = (take half xs, drop half xs, Nothing)
        go (half, _) = (take half ys, drop half ys, Just y)

naive :: [Int] -> [Int]
naive xs = case mextra of
    Nothing -> flatten zippeds
    Just x -> x : flatten zippeds
    where
        sorteds = sort xs
        (lowers, uppers, mextra) = splitEvenly sorteds
        zippeds = zip lowers uppers

-- The performance of this implementation is bound by
-- the sort operation which, depending on implementation,
-- will be approximately O(n log n).

-- The challenge suggests we can do it in O(n) and/or with O(1) extra space.
-- The O(1) extra space will come from performing operations in place
-- with one or two extra allocations to temporarily hold values while moving them.
-- We'll forgoe that here since doing things in-place in Haskell, while
-- possible, is not standard.
--
-- As for the hunt for an O(n) solution...
-- We're clearly searching for a permutation of xs that
-- meets our criteria.
--
-- Due to laziness in Haskell, and the fact that many
-- candidates will rules themselves out quickly, the
-- performance of filtering permutations might even
-- prove satisfactory. Suppose we modelled the permutations
-- as a tree where a node represented the current element
-- and the children of the node represented choices of next
-- element. This would allow us to use laziness to rule out
-- huge numbers of permutations without computing them.
--
-- Either way, we can likely still do better. Let's consider
-- a few small cases. The problem is clearly recursive since
-- removing one element still yields a correct result.

-- [] = []
-- [1] = [1]
-- [1,2] = [1,2]
-- [2,1] = [1,2] -- swap the elements
-- [1,2,3] = [2,1,3]
--
-- iterating over and swapping elements won't work,
-- put we could view the problem differently...
-- Our "bottleneck" is currently the sort function.
-- We don't need the list fully sorted, we just
-- need to split the uppers and lowers. What that
-- actually means is iterating through and collecting
-- length n / 2 largest elements. This can be done in O(n)
-- time with a single pass, rather than O(n log n) time.
--
-- We can do this by finding the median, which can be
-- done in O(n) expected time via QuickSelect.

-- In practice, I'd also want to input a NonEmpty
quickSelect :: Int -> [Int] -> Int
quickSelect _ [] = error "No k-th element for empty list"
quickSelect k0 xs0 = evalState (go k0 xs0) (mkStdGen 1337)
    where
        go :: Int -> [Int] -> State StdGen Int
        go k xs = do
            gen0 <- get

            let (pivotIx, gen1) = randomR (0, length xs - 1) gen0
                pivot = xs !! pivotIx

            put gen1

            -- In practice we could write a function to combine
            -- these into a single pass for efficiency, we could
            -- also count the length as we construct the lists.
            let (lesses, notLesses) = partition (< pivot) xs
                (eqs, mores) = partition (== pivot) notLesses

            let lenL = length lesses
                lenE = length eqs

            case () of
                _
                    | k < lenL -> go k lesses
                    | k > lenL + lenE -> go (k - lenL - lenE) mores
                    | otherwise -> pure pivot

median :: [Int] -> Int
median xs = quickSelect (length xs `div` 2) xs

wiggleSort :: [Int] -> [Int]
wiggleSort xs
    | length lowers == length uppers = flatten (zip lowers uppers)
    | otherwise = flatten (zip ls uppers) ++ [l]
    where
        (lowers@(l:ls), uppers) = partition (<= median xs) xs

