{-# LANGUAGE DerivingStrategies #-}

module MinimiseLoss where

import Control.Monad.State (MonadState (..), execState)
import Data.Set (Set)
import Data.Set qualified as Set

data S = S
    { buys :: Set Integer
    , loss :: Integer
    }
    deriving stock (Show)

-- Given a sell price and a set of buy prices,
-- what's the smallest, greater than zero, loss?
smallestLoss :: Integer -> Set Integer -> Maybe Integer
smallestLoss sell buys =
    fmap (\b -> b - sell) $
        Set.lookupGT sell buys

minLoss :: [Integer] -> Integer
minLoss [] = 0
minLoss (buy0 : xs) = loss $ execState (go xs) initState
    where
        initState = S (Set.singleton buy0) buy0
        go [] = pure ()
        go (p : ps) = do
            S{buys, loss} <- get
            -- if we sell at price p,
            -- what's the best price we could have bought at
            -- such that we make the smallest loss
            let mLoss = smallestLoss p buys
                bestLoss = case mLoss of
                    Nothing -> loss
                    Just newLoss -> min loss newLoss

            put $ S (Set.insert p buys) bestLoss

            go ps
