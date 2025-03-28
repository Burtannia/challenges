module NewYearChaos where

import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed ((!))


data Bribe = TooChaotic | Ok Int

instance Semigroup Bribe where
    TooChaotic <> _ = TooChaotic
    _ <> TooChaotic = TooChaotic
    (Ok n) <> (Ok m) = Ok (n + m)

instance Monoid Bribe where
    mempty = Ok 0

minimumBribes :: [Int] -> IO ()
minimumBribes q0 = case res of
    TooChaotic -> putStrLn "Too chaotic"
    Ok n -> print n
    where
        q = V.fromList q0
        res = mconcat $ map check [1 .. V.length q]
        check pos
            -- Person in this position has a
            -- label with a difference (from
            -- the expected) greater than two.
            | self - pos > 2 = TooChaotic
            -- Otherwise, count how many "higher"
            -- people are ahead of where this person
            -- started. In other words, how many
            -- people have given bribes to this person?
            | otherwise = Ok
                $ length
                $ filter (> self)
                -- This indexing logic is extremely tricky.
                -- If somebody bribed us, the question assumes
                -- that we cannot bribe them back, so they must
                -- be ahead of our label/where we started (self).
                --
                -- The person behind us (self + 1) could only have
                -- bribed twice and reached (self - 1), so we start
                -- our scan there. We then finish our scan at the
                -- position in front of our current position (pos - 1).
                $ [ q ! (p - 1) | p <- [max 1 (self - 1) .. pos - 1] ]
            where
                self = q ! (pos - 1)
