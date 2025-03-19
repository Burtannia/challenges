module QueensAttack where

-- We can trivially calculate the
-- up/down, left/right and daigonal
-- squares, up to the edge of the
-- board.
--
-- We can calculate up to the edge
-- of the board OR an obstacle.
--
-- This would require frequent
-- lookup of obstacles which could
-- be done in expected O(1) time
-- via a HashMap.
--
-- There might be multiple obstacles
-- on each square, but we don't care
-- about that. A HashSet would be fine.

-- Alternatively, we can prboably do
-- better by calculating the length
-- of each path the queen sees.
--
-- Then we find the minimum obstacle
-- for each line and subtract the difference.
--
-- We should bucket our obstacles by
-- direction when we store them in the
-- set and store a set for each direction.
-- The directions can be stored in a
-- vector for efficient retrieval.
--
-- 0 is up, 1 is top-right diagonal, so on...

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Heap (Heap)
import Data.Heap qualified as H
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Maybe (fromMaybe)

type Square = (Int, Int)

directionIx :: NSquare -> Int
directionIx (NSquare (0, y))
    | y > 0 = 0
    | otherwise = 4
directionIx (NSquare (x, 0))
    | x > 0 = 2
    | otherwise = 6
directionIx (NSquare (x, y))
    | x > 0 && y > 0 = 1
    | x > 0 = 3
    | x < 0 && y < 0 = 5
    | otherwise = 7

-- we must be careful to normalise the direction
-- based on the queen's position.

newtype NSquare = NSquare Square

normalise :: Square -> Square -> NSquare
normalise (qx, qy) (x, y) = NSquare (x - qx, y - qy)

inSight :: NSquare -> Bool
inSight (NSquare (x, y)) =
    abs x == abs y
        || x == 0
        || y == 0

-- after thought, we don't need to store the obstacles, just
-- their distance, but we will want the minimum so a heap
-- would be good.

distance :: NSquare -> Int
distance (NSquare (0, y)) = abs y
distance (NSquare (x, _)) = abs x -- since we only care about inSight squares

toSquare :: [Int] -> Square
toSquare [x, y] = (x, y)

storeObstacles :: Square -> [[Int]] -> Vector (Heap Int)
storeObstacles queen os = runST $ do
    let nos = map (normalise queen . toSquare) os
    mv <- MV.replicate 8 H.empty

    -- for each normalised obstacle,
    -- if it's inSight, add its distance to the direction ix
    forM_ nos $ \nsq ->
        when (inSight nsq) $
            MV.modify mv (H.insert $ distance nsq) (directionIx nsq)

    V.freeze mv

queenMaxView :: Int -> Int -> Square -> Int
queenMaxView 0 n (_, qy) = n - qy
queenMaxView 1 n (qx, qy) = min (n - qx) (n - qy)
queenMaxView 2 n (qx, _) = n - qx
queenMaxView 3 n (qx, qy) = min (n - qx) (qy - 1)
queenMaxView 4 _ (_, qy) = qy - 1
queenMaxView 5 _ (qx, qy) = min (qx - 1) (qy - 1)
queenMaxView 6 _ (qx, _) = qx - 1
queenMaxView 7 n (qx, qy) = min (qx - 1) (n - qy)

view :: Int -> Square -> Vector (Heap Int) -> Int -> Int
view n queen obs d =
    fromMaybe (queenMaxView d n queen) maybeObsDist
    where
        dirObs = obs V.! d
        maybeObsDist
            | H.null dirObs = Nothing
            | otherwise = Just (H.minimum dirObs - 1)

-- for each direction, fetch the minimum obstacle distance
-- if the heap is empty, calculate the furthest the queen
-- can see to the edge of the board
queensView :: Int -> Square -> [[Int]] -> Int
queensView n queen os =
    foldl'
        (\acc d -> acc + view n queen obsDistances d)
        0
        [0 .. 7]
    where
        obsDistances = storeObstacles queen os
