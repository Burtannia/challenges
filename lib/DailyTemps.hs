{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DailyTemps where

import Control.Monad.State (MonadState (..), State, execState)

type Stack a = [a]

push :: (Ord a) => a -> Stack a -> Stack a
push x [] = [x]
push x (y : ys)
    | x >= y = push x ys
    | otherwise = x : y : ys

-- [73]
-- push 76
-- pop 73, [76]
-- length [73] == length [76], popped 1
--
-- [69, 72]
-- push 71
-- pop 69, [71, 72]
-- length [69, 72] == length [71, 72], popped 1
--
-- [69, 71, 72, 76]
-- push 75
-- pop 69, 71, 72, [75,76]
-- length from 4 to 2, popped 3
-- relationship == 1 + old len - new len

-- [73]
-- [76]
-- [72, 76]
-- [69, 72, 76]
-- [71, 72, 76]
-- push 75, problem is 69 was already popped by 71
-- so we don't care about the number of popped answers
-- we care about peaking to the second element and finding
-- its index. So let's push pairs to the stack instead.

data Indexed a = Indexed
    { index :: Int
    , value :: a
    }
    deriving stock (Show)

instance (Eq a) => Eq (Indexed a) where
    x == y = x.value == y.value

instance (Ord a) => Ord (Indexed a) where
    x <= y = x.value <= y.value

-- Get's the absolute gap between the indexes
-- of the top two elements in the stack,
-- if the stack contains fewer than 2 elements,
-- returns 0
getGap :: Stack (Indexed Int) -> Int
getGap [] = 0
getGap [_] = 0
getGap (x : y : _) = abs (y.index - x.index)

withIndexes :: [a] -> [Indexed a]
withIndexes xs = map (uncurry Indexed) $ zip [0 ..] xs

dailyTemps :: [Int] -> [Int]
dailyTemps temps = snd $ execState (go ixedTemps) ([], [])
    where
        ixedTemps = withIndexes temps
        go :: [Indexed Int] -> State (Stack (Indexed Int), [Int]) ()
        go [] = pure ()
        go (x : xs) = do
            go xs
            (stk0, res0) <- get
            let stk1 = push x stk0
                gap = getGap stk1
                res1 = gap : res0
            put (stk1, res1)

-- This fails when there are consecutive temperatures:
-- [70,71,71,71,72]
-- should produce [1,3,2,1,0]
-- instead it produces [1,1,1,1,0]

-- [72], no next element so we push 0 to the results
-- [71,72], push 1 to the results
-- [71,71,72], we want to pop equal elements...
-- fixed by changing (>) in 'push' to (>=)
