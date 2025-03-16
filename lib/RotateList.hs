{-# LANGUAGE OverloadedRecordDot #-}

module RotateList where

import Prelude hiding (length, splitAt)

data ListNode a = ListNode
    { val :: a
    , next :: Maybe (ListNode a)
    }

toList :: ListNode a -> [a]
toList node = case node.next of
    Nothing -> [node.val]
    Just nextNode -> node.val : toList nextNode

fromList :: [a] -> ListNode a
fromList [] = error "Cannot create empty ListNode"
fromList [x] = ListNode x Nothing
fromList (x:xs) = ListNode x (Just $ fromList xs)

instance (Show a) => Show (ListNode a) where
    show = show . toList

instance Semigroup (ListNode a) where
    x <> y = case x.next of
        Nothing -> ListNode x.val (Just y)
        Just nextNode -> ListNode x.val (Just $ nextNode <> y)

length :: ListNode a -> Int
length = go 1
    where
        go n node = case node.next of
            Nothing -> n
            Just nextNode -> go (n + 1) nextNode

splitAt :: Int -> ListNode a -> (ListNode a, Maybe (ListNode a))
splitAt 0 node = (ListNode node.val Nothing, node.next)
splitAt n node0 = case node0.next of
    Nothing -> (node0, Nothing)
    Just nextNode ->
        let (node1, mrest) = splitAt (n - 1) nextNode
            node2 = ListNode node0.val (Just node1)
         in (node2, mrest)

actionsNeeded :: Int -> ListNode a -> Int
actionsNeeded n node = n `mod` (length node)

rotate :: Int -> ListNode a -> ListNode a
rotate k0 node =
    let k1 = actionsNeeded k0 node
        (newEnd, mx) = splitAt k1 node
     in case mx of
            Nothing -> newEnd
            Just newFront -> newFront <> newEnd

rotateFromList :: Int -> [a] -> ListNode a
rotateFromList k xs = rotate k (fromList xs)
