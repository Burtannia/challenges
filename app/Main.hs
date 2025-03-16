module Main where

import Challenges (name)

main :: IO ()
main = putStrLn ("Hello " <> name)
