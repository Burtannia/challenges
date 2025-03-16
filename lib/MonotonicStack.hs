{-# LANGUAGE DataKinds #-}

module MonotonicStack where

data Direction = Asc | Desc

data Stack (d :: Direction) a = Stack [a]

pop :: Stack d a -> (Maybe a, Stack d a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

top :: Stack d a -> Maybe a
top = fst . pop


