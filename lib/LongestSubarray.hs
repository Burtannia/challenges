{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DerivingStrategies #-}

module LongestSubarray where

import Control.Monad (when)
import Control.Monad.State (MonadState (..), execState)
import Data.Vector (Vector)
import Data.Vector qualified as V

longestSubarray :: [Int] -> Int -> [Int]
longestSubarray xs k = take (end - start + 1) $ drop start xs
    where
        (start, end) = longestSubarray_ (V.fromList xs) k

data SubarrayState = S
    { left :: Int
    , right :: Int
    , total :: Int
    , best :: (Int, Int)
    , bestGap :: Int
    }
    deriving stock (Show)

longestSubarray_ :: Vector Int -> Int -> (Int, Int)
longestSubarray_ xs k = sFinal.best
    where
        maxIx = V.length xs - 1
        sFinal = execState goRight (S 0 0 (xs V.! 0) (0, 0) 0)

        shrinkLeft = do
            s <- get
            if s.left == s.right
                then pure ()
                else do
                    let left = s.left + 1
                        total = s.total - (xs V.! s.left)
                    put $ s{left, total}
                    if total > k then shrinkLeft else pure ()

        goRight = do
            s <- get
            if s.right == maxIx
                then pure ()
                else do
                    let newRight = s.right + 1
                        newTotal = s.total + (xs V.! s.right)
                    put $ s{right = newRight, total = newTotal}

                    when (newTotal > k) shrinkLeft

                    s1 <- get
                    let newGap = (right s1) - (left s1)

                    when (s1.total <= k && newGap > (bestGap s1)) $
                        put $
                            s1{best = (s1.left, s1.right), bestGap = newGap}

                    goRight
