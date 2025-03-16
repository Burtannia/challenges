{-# LANGUAGE BlockArguments #-}

module NextGreaterElem where

import Control.Monad.State (MonadState (..), State, execState)
import Data.HashMap.Strict (HashMap, empty, insert, (!))

-- O(n)
afters :: Int -> [Int] -> [Int]
afters x xs = dropWhile (/= x) xs

-- -1 if none
-- O(n)
nextGreater :: Int -> [Int] -> Int
nextGreater _ [] = -1
nextGreater n (x : xs)
    | x > n = x
    | otherwise = nextGreater n xs

-- O(n)
-- Won't ever actually be O(2n) since
-- if 'afters' is worst case, then 'nextGreater'
-- will be best case of o(1).
nextGreaterAfter :: Int -> [Int] -> Int
nextGreaterAfter n xs = nextGreater n (afters n xs)

-- O(m*n) where m is nums1.len and n is nums2.len
nextGreaters :: [Int] -> [Int] -> [Int]
nextGreaters nums1 nums2 = map (\n -> nextGreaterAfter n nums2) nums1

-- Given that all integers in the list are unique, we can do better.
-- Given (x:y:xs) as nums1, where n is the next greater for x,
-- if y < x -> not helpful, there could be something between y and x
-- if y > x -> not massively helpful, unless y < n in which case n holds.
--
-- maybe going backwards helps?
-- if n is the next greater for y...
-- if x < y -> y is the next greater for x
-- if x > y -> not massively helpful unless x < n in which case n holds.
--
-- Suppose we had some magical lookup table with answers in it,
-- we would still need to lookup an answer for each element of nums1
-- hence we can suggest a limit on efficiency of O(m) where m = nums1.len
--
-- How can we compute such a lookup table? This would provide us with
-- O(m + x) where x is the cost of computing the table.
--
-- Since nums1 is a subset of nums2, we can simply computer the results
-- for each element in nums2. Done naively, this has the same time complexity
-- but slightly worse in that it's now O(m + n*n) ~ O(n^2).
--
-- Since we're interested in elements after the element we're looking at,
-- working backwards gives us the right "vision". Pushing elements into a
-- monotonic stack then gives us the ordering knowledge.
--
-- This would give a result of O(m+n). Due to them being subsets,
-- m <= n so O(m+n) has an upper bound of O(2n) ~= O(n).

type Stack a = [a]

push :: (Ord a) => a -> Stack a -> Stack a
push x [] = [x]
push x (y : ys)
    | x > y = push x ys
    | otherwise = (x : y : ys)

peakSnd :: Stack Int -> Int
peakSnd [] = -1
peakSnd [_] = -1
peakSnd (_ : x : _) = x

-- Theoretically a HashMap could degrade and give us O(n^2)
-- if the lookups all take O(n). But assuming a sane O(1)
-- average this should be fine. If we wanted to be sure of
-- O(1) in all cases then we could assign a Vector with 1001
-- elements and use the number as the index, since 1000 is
-- the maximum element. But this is marginally insane due to
-- the massively increased memory cost (unless we can be sure
-- that our input lists will contain close to 1000 elements).
computeEntry :: Int -> State (Stack Int, HashMap Int Int) ()
computeEntry n = do
    (stk0, tbl) <- get
    let stk1 = push n stk0
        ans = peakSnd stk1
    put (stk1, insert n ans tbl)

computeTable :: [Int] -> HashMap Int Int
computeTable xs = snd $ execState (go xs) ([], empty)
    where
        go [] = pure ()
        go (y : ys) = do
            go ys
            computeEntry y

fastNextGreaters :: [Int] -> [Int] -> [Int]
fastNextGreaters nums1 nums2 =
    map (table !) nums1
    where
        table = computeTable nums2
