module NonDivSubset where

import Data.Vector (Vector)
import Data.Vector qualified as V

-- Naively generating all permutations
-- would be O(n!) *yikes*.
--
-- We would then have to perform an
-- O(n) check for each subset.
--
-- This would lead to O(n! * n)...

-- We needn't generate every permutation.
-- Once we find a permutation of length x,
-- for which the sum is not divisible by k,
-- we can proceed to check permutations x+1.
--
-- Likewise if we fail for all permutations of
-- x + 1, then we need not check x + 2.
--
-- While, in the worst case this would still
-- have the same complexity, in reality, it's
-- likely to be significantly better.

-- The only optimisation here would be to use
-- a DList to delay the append.
permsNoDupsN :: Int -> [Int] -> [[Int]]
permsNoDupsN 0 _ = [[]]
permsNoDupsN _ [] = []
permsNoDupsN n (x : xs) =
    map (x :) (permsNoDupsN (n - 1) xs)
        ++ permsNoDupsN n xs

newtype DList a = DList
    {runDList :: [a] -> [a]}

instance Semigroup (DList a) where
    f <> g = DList (runDList f . runDList g)

instance Monoid (DList a) where
    mempty = DList id

(.:) :: a -> DList a -> DList a
x .: f = DList $ \xs -> x : runDList f xs

dlistToList :: DList a -> [a]
dlistToList dl = runDList dl []

singleton :: a -> DList a
singleton x = x .: mempty

instance Functor DList where
    fmap f dl = DList $ \xs ->
        (map f $ dlistToList dl) <> xs

permsBest :: Int -> [Int] -> [[Int]]
permsBest n0 xs0 = dlistToList $ go n0 xs0
    where
        go :: Int -> [Int] -> DList [Int]
        go 0 _ = singleton []
        go _ [] = mempty
        go n (x : xs) =
            fmap (x :) (go (n - 1) xs)
                <> go n xs

twos :: [Int] -> [[Int]]
twos [] = []
twos (x : xs) =
    [[x, y] | y <- xs] ++ twos xs

-- 'any' should provide short-circuiting behaviour.
anyTwoDivK :: Int -> [Int] -> Bool
anyTwoDivK k xs =
    any
        (\two -> sum two `mod` k == 0)
        (twos xs)

-- We are looking for a maximum length n
-- such that, there exists a permutation, S',
-- of n elements from S, for which no
-- two digits from S' sum to a number divisible
-- by k.
maxNonDiv :: Int -> [Int] -> Int
maxNonDiv k xs = go 2
    where
        go n
            | n > length xs = -1
            | otherwise =
                if hasNonDivisible
                    then go (n + 1)
                    else n - 1
            where
                ps = permsBest n xs
                hasNonDivisible = any (not . anyTwoDivK k) ps

-- These solutions are still categorically suboptimal
-- so regardless of how much we optimise them, they
-- still won't be feasible for large inputs.
--
-- We could improve our search by randomness. In other
-- words, we choose a random value for the arity of the
-- subsets. If there is a non divisible permutation then
-- we know the solution lies in larger permutations or
-- vice versa. This is in the same category as QuickSort.
--
-- Alternatively, perhaps we can construct the maximal subset,
-- by exploiting some other knowledge about the problem.
-- This transforms the problem away from searching and should
-- reduce the complexity.
--
-- We're adding two numbers together from the subset,
-- so any subset containing two multiples of k will
-- fail the test.
--
-- If the modulos of any two numbers add up to k (or 0)
-- then we cannot have them both.

-- For a value k, returns a vector of length k
-- where V[i] is a list of elements from xs
-- for which x `mod` k == i.
groupMods :: Int -> [Int] -> Vector [Int]
groupMods k = go (V.fromList $ replicate k [])
    where
        go v [] = v
        go v0 (x : xs) = go v1 xs
            where
                v1 = V.update v0 us
                ix = x `mod` k
                us = V.fromList [(ix, x : v0 V.! ix)]

-- From modulo 0, we can take at most 1 element
-- From modulo 1 and k - 1, we can take at most 1 element
-- From modulo 2 and k - 2, we can take at most 1 element
-- If k is even, then we can also only take at most 1 from k/2.
--
-- So we actually only need buckets up to k/2 and we include
-- any elements from m and k - m. If k is even then k/2 is its
-- own bucket.
--
-- But we could take 2 from modulo = 1 and none from k - 1.

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

maxSubset :: Int -> [Int] -> Int
maxSubset k xs = sum scores
    where
        count i = length $ buckets V.! i
        buckets = groupMods k xs
        scores = map modScore [0 .. k `div` 2]
        modScore 0 = min (count 0) 1
        modScore n
            | isEven k && n == k `div` 2 =
                min (count (k `div` 2)) 1
            | otherwise =
                max (count n) (count (k - n))
