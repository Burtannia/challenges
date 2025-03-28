module CaesarCipher where

import Data.Char (isLower, isUpper)

transform :: Int -> Char -> Char
transform n c
    | isUpper c =
        let ix = fromEnum c - 65
            newIx = (ix + n) `mod` 26
         in toEnum (newIx + 65)
    | isLower c =
        let ix = fromEnum c - 97
            newIx = (ix + n) `mod` 26
         in toEnum (newIx + 97)
    | otherwise = c

caesarCipher :: String -> Int -> String
caesarCipher plain n = map (transform n) plain

perms2 :: [a] -> [[a]]
perms2 [] = [[]]
perms2 xs = [x : zs | (x,ys) <- picks xs, zs <- perms2 ys]

picks :: [a] -> [(a, [a])]
picks = undefined
