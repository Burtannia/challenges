{-# LANGUAGE DerivingStrategies #-}

module ClimbingLeaderboard where

import Control.Monad.State.Strict (MonadState (..), execState)

rank :: [Int] -> Int -> Int
rank = flip $ go 1
    where
        go n _ [] = n
        go n score (r : rs)
            | score >= r = n
            | otherwise = go (n + 1) score (dropWhile (== r) rs)

ranks :: [Int] -> [Int] -> [Int]
ranks rs ps = map (rank rs) ps

-- This implementation was rejected as "too inefficient"
-- by Hackerrank...
--
-- This is currently O(n*m) where n is the size of the
-- leaderboard and m is the number of player scores
-- to rank.
--
-- The only thing we can really use to improve the
-- efficiency is that the leaderboard is in descending
-- order and the player's scores are in ascending order.
--
-- We can potentially reduce the runtime to O(n + m) by
-- iterating from the bottom of the leaderboard to the
-- top. Since the player's scores are in ascending order
-- we will never have to backtrack.
--
-- We'll probably want to create a compressed stack-like
-- structure to remove duplicates.

-- Maintains an ascending ranked list of 'a'
-- with no duplicates. Lesser elements are
-- popped off the stack on push.
--
-- In this sense, RankedStack is a form of
-- monotonic stack.
data RankedStack a = Stack [(Int, a)]
    deriving stock (Show)

emptyStack :: RankedStack a
emptyStack = Stack []

push :: (Ord a) => a -> RankedStack a -> RankedStack a
push x (Stack []) = Stack [(1, x)]
push x (Stack ((r, y) : ys))
    | x < y = Stack (((r + 1, x) : (r, y) : ys))
    | otherwise = push x (Stack ys)

-- For all input elements to exist in the
-- stack (minus duplicates), the input list
-- must be in descending order.
mkStack :: (Ord a) => [a] -> RankedStack a
mkStack = foldl' (flip push) emptyStack

unsafePeak :: RankedStack a -> (Int, a)
unsafePeak (Stack []) = error "empty stack"
unsafePeak (Stack (x : _)) = x

bottomRank :: RankedStack a -> Int
bottomRank = fst . unsafePeak

-- We could also use a DList with appends
-- to avoid the need to reverse afterwards.
fastRanks :: [Int] -> [Int] -> [Int]
fastRanks rs ps0 = reverse $ fst $ execState (go ps0) initState
    where
        initState = ([], rstack0)
        rstack0 = mkStack rs
        go [] = pure ()
        go (p : ps) = do
            (scrs, stk0) <- get
            let stk1 = push p stk0
                scr = bottomRank stk1
            put (scr : scrs, stk1)
            go ps

-- Given that we know the input list is perfectly
-- ordered, mkStack will run in O(n) rather than
-- the worst case of O(n^2). Due to the ascending
-- nature of the player scores, we pop off any
-- previously surpassed elements meaning this
-- should all be O(n + m).
