module SubarrayMins where

-- [3, 1, 2, 4]
-- only look at the case starting with 3 for now
--
-- look at 3 : xs, [3]
-- now pass the next call the previous min of 3 and xs
-- look at 1 : xs, 1 is less than the previous min of 3
-- so we take it as the new min and pass it along
minsOfSubarrays :: [Int] -> [Int]
minsOfSubarrays [] = []
minsOfSubarrays (x:xs) = x : go x xs
    where
        go _ [] = []
        go prevMin (y:ys)
            | y < prevMin = y : go y ys
            | otherwise = prevMin : go prevMin ys

minsOfAllSubarrays :: [Int] -> [Int]
minsOfAllSubarrays [] = []
minsOfAllSubarrays ys@(_:xs) =
    minsOfSubarrays ys <> minsOfAllSubarrays xs

sumOfMins :: [Int] -> Int
sumOfMins = sum . minsOfAllSubarrays

-- naturally we could make this a bit more
-- efficient by not constructing the lists
-- and just summing as we go, but this way
-- breaks things down nicely in my opinion.

-- This is still O(n^2) since we iterate over
-- ever set, an improvement over O(n^3) but
-- still not great.

-- The minimum element for an given subarray
-- has to be one of the elements in the total
-- set. So the sum will be of the form
-- c0s0 + c1s1 ... where c0 is the number of
-- subsets for which s0 is the minimum.

-- [3], min for 1 subarray
-- [2,3], 2 is the min for 2 subarrays
-- [4,2,3], 2 is now the min for an additional subarray,
-- 4 is the minimum for 1 subarray
--
-- walking backwards doesn't seem to help
-- when we start at the beginning, we only
-- have a single, whole subarray to cover,
-- then on the second step, we have two subarrays
-- to cover etc. Becuase we go from n elements
-- to n-1 etc.
--
-- We can increment a counter and multiply that
-- by the minimum value at each step.

type Stack a = [a]

push :: Ord a => a -> Stack a -> Stack a
push x [] = [x]
push x (y:ys)
    | x >= y = push x ys
    | otherwise = x : y : ys

unsafePeak :: Stack a -> a
unsafePeak [] = error "empty stack"
unsafePeak (x:_) = x

-- fastSumOfMins :: [Int] -> Int
-- fastSumOfMins = go (1, [], 0)
--     where
--         go :: (Int, Stack Int, Int) -> [Int] -> Int
--         go (_, _, sum_) [] = sum_
--         go (n, stk0, sum0) (x:xs) =
--             let stk1 = push x stk0
--                 sum1 = sum0 + n * (unsafePeak stk1)
--              in go ((n + 1), stk1, sum1) xs
        




