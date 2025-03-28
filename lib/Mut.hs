module Mut where

import Control.Monad.ST
import Data.STRef
import Control.Monad

sumInplace :: [Int] -> Int
sumInplace xs = runST $ do
    -- runST ensures the mutability
    -- remains contained in this scope.

    -- It's a mutable var in pure code!
    n <- newSTRef 0

    forM_ xs $ \x -> do
        modifySTRef n (+ x)

    -- effectively Rust's `.clone()`
    -- creates a pure copy.
    readSTRef n
