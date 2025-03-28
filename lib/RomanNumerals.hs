{-# LANGUAGE OverloadedRecordDot #-}

module RomanNumerals where

import Data.Set (Set)
import Data.Set qualified as Set

data State = State
    { prev :: Int
    , total :: Int
    , adding :: Bool
    }

emptyState :: State
emptyState = State 0 0 True

addState :: Int -> State -> State
addState x st = State x (st.total + x) True

subState :: Int -> State -> State
subState x st = State x (st.total - x) False

newtype RomanNumeral = RomanNumeral String

parseRomanNumeral :: String -> Maybe RomanNumeral
parseRomanNumeral str
    | all validRomanChar str = Just (RomanNumeral str)
    | otherwise = Nothing

validChars :: Set Char
validChars = Set.fromList ['I', 'V', 'X', 'L', 'C', 'D', 'M']

validRomanChar :: Char -> Bool
validRomanChar c = Set.member c validChars

charToDecimal :: Char -> Int
charToDecimal 'I' = 1
charToDecimal 'V' = 5
charToDecimal 'X' = 10
charToDecimal 'L' = 50
charToDecimal 'C' = 100
charToDecimal 'D' = 500
charToDecimal 'M' = 1000

toDecimal :: RomanNumeral -> Int
toDecimal (RomanNumeral str) = finalState.total
    where
        finalState = foldr f emptyState str
        f c st
            | st.adding && cVal < st.prev = subState cVal st
            | st.adding = addState cVal st
            | cVal > st.prev = addState cVal st
            | otherwise = subState cVal st
            where
                cVal = charToDecimal c

romanToDecimal :: String -> Int
romanToDecimal str = case parseRomanNumeral str of
    Nothing -> error ("Invalid Roman numeral: " <> str)
    Just rn -> toDecimal rn
