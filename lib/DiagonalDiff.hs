module DiagonalDiff where

type Matrix = [Row]
type Row = [Int]

diags :: Matrix -> ([Int], [Int])
diags rs =
    unzip $
        [ (r !! ix, r !! (l - ix - 1))
        | (r, ix) <- zip rs [0 ..]
        , let l = length r
        ]

diagonalDifference :: Matrix -> Int
diagonalDifference m = abs (sum d1 - sum d2)
    where
        (d1, d2) = diags m

test :: Matrix
test =
    [ [1, 2, 3]
    , [4, 5, 6]
    , [9, 8, 9]
    ]

