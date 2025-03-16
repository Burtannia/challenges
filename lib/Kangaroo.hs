{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Kangaroo where

data Kangaroo = Kangaroo
    { speed :: Int
    , start :: Int
    }
    deriving stock (Show)

-- if the faster kangaroo is ever in front
-- then the kangaroos will never meet
--
-- if both kangaroos have the same speed,
-- unless they start on the same spot,
-- they will never meeet.
--
-- let K1vals be the set of multiples of
-- K1speed offset by K1start.
--
-- let K2vals be the set of multiples of
-- K2speed offset by K2start.
--
-- Let K1 be the faster of the two.
--
-- We can iterate through K1
-- looking for multiples of K2speed with K2start offset.

naive :: Kangaroo -> Kangaroo -> Bool
naive k1 k2
    | k1.speed < k2.speed = naive k2 k1
    | k1.speed == k2.speed = k1.start == k2.start
    | otherwise = go k1s k2s
    where
        k1s = [k1.start, k1.start + k1.speed ..]
        k2s = [k2.start, k2.start + k2.speed ..]
        go (k1x : k1xs) (k2x : k2xs)
            | k1x == k2x = True
            | k1x > k2x = False
            | otherwise = go k1xs k2xs
        go _ _ = False

naiveKangaroos :: Int -> Int -> Int -> Int -> String
naiveKangaroos x1 v1 x2 v2
    | naive k1 k2 = "YES"
    | otherwise = "NO"
    where
        k1 = Kangaroo v1 x1
        k2 = Kangaroo v2 x2

-- We can probably do much better by manipulating
-- the numbers more directly.
--
-- We're interested in where the two lines intersect.
-- Naturally we know that the two lines will cross,
-- unless in the trivial case that the faster kangaroo
-- starts out ahead.
--
-- So we simply wish to determine whether said cross
-- point is a member of both the sets of jump points
-- i.e. a multiple of the speed given the offset.
--
-- It's almost easier to visualise if the kangaroos
-- are moving in two dimensions rather than one...
--
-- Perhaps we want to compute the lowest common multiple
-- of the two speeds, given an offset.
--
-- We can calculate when the number of jumps it will
-- take for the faster kangaroo to overtake, given the
-- gap we divide it by the difference in velocity.
--
-- K1pos(x) = K1start + x * K1speed
-- K2pos(x) = K2start + x * K2speed
-- K1start + x * K1Speed = K2start + x * K2speed
-- x * K1Speed - x * K2Speed = K2start - K1start
-- x (K1speed - K2speed) = K2start - K1start
-- x = K2start - K1start / K1speed - K2speed
--
-- providing the speeds are not equal (which is a trivial case)
-- x should be a positive whole integer if the kangaroos meet.

kangaroos :: Int -> Int -> Int -> Int -> String
kangaroos x1 v1 x2 v2
    | meet = "YES"
    | otherwise = "NO"
    where
        meet
            | v1 == v2 = x1 == x2
            | otherwise = case calcJumps of
                (n, 0) -> n > 0
                _ -> False
        calcJumps = (x2 - x1) `divMod` (v1 - v2)
