module TowerBreakers where

-- Minimax provides a nice algorithm
-- for solving two player games, but
-- it seems unnecessary in this case.
--
-- We know that if there are an even
-- number of moves then player 2 will win.
-- Otherwise, player 1 will win.


-- Player 1: odd
-- Player 2: even

-- Let's consider towers with odd heights...
--
-- 7 -> 1 (always odd, since prime), player1 will win
--
-- 15 -> 5 -> 1 (even)
-- 15 -> 3 -> 1 (even)
-- 15 -> 1 (odd), player1 will force this move first
--
-- 21 -> 7 -> 1 (even)
-- 21 -> 3 -> 1 (even)
-- 21 -> 1 (odd), player1 will force this move first
-- player1 cannot win

-- It appears, for odd towers, player1 can always force a win
-- For multiple odd towers, it still doesn't matter what player2
-- does since, if player2 changes a different tower, it just
-- produces another odd number.

-- Let's consider towers with even heights...
-- Player1 will trivially win for only 1 tower,
-- so we will consider 2.
--
-- (4,4) -> (2,4) -> (1,4) -> (1,2) -> (1,1), (even), player2 can force this or the next
-- (4,4) -> (2,4) -> (2,2) -> (1,2) -> (1,1), (even)
-- (4,4) -> (2,4) -> (2,1) -> (1,1), (odd)
--
-- (4,4) -> (1,4) -> (1,2) -> (1,1), (odd)
-- (4,4) -> (1,4) -> (1,4) -> (1,2) -> (1,1), (even), player2 will force this
-- (4,4) -> (1,4) -> (1,4) -> (1,1), (odd)
--
-- (4,4,4) -> (2,4,4) -> (1,4,4) -> (1,2,4) -> (1,1,4) -> (1,1,1), (odd)
-- (4,4,4) -> (2,4,4) -> (1,4,4) -> (1,2,4) -> (1,1,4) -> (1,1,2) -> (1,1,1), (even)
-- (4,4,4) -> (2,4,4) -> (1,4,4) -> (1,1,4) -> (1,1,2) -> (1,1,1), (odd)
-- (4,4,4) -> (2,4,4) -> (1,4,4) -> (1,1,4) -> (1,1,1), (even)
-- (4,4,4) -> (2,4,4) -> (1,4,4) -> (1,1,2) -> (1,1,1), (even)
--
-- ... this is taking forever, let's reason a bit more abstractly...
--
-- if we have odd towers of even heights, then player1 would win if
-- each move took a tower to 1. Also, if reducing a tower took an
-- odd number of steps, player 1 would win. So player 2 must find
-- a way to force an even number of moves.
--
-- With an even height, player2 can either:
--     put it to 1 -> not helpful
--     put it to an odd factor, if one exists -> not helpful
--     put it to 2 -> always helpful
--
-- If player1 continues reducing towers to 1, they will eventually
-- lose due to the 2 high tower. So player1 must counter play by
-- also putting a tower to 2. This "cancels out" player2's move
-- and we're back to where we started. With an odd number of towers
-- player1 will definitely win.
--
-- With an even number of even towers... Player1 can't follow the
-- "reduce to 1" tactic. Instead, they can reduce the even number
-- to another even number. Player2 can simply mimic this and will
-- win.
--
-- If player 1 reduces a tower to an odd number (assuming there is one),
-- then they will also lose since player 2 will just mimic their
-- moves once again.

-- This is much simpler than all this, the game is first player
-- advantage for an odd number of towers. With an even number
-- of towers, player2 can just mimic player1 and win.
--
-- There is no need to analyse individual moves.

towerBreakers :: Int -> Int -> Int
-- trivially, if all the towers have a height of 1, then player 2 wins
towerBreakers _ 1 = 2
towerBreakers n _
    | n `mod` 2 == 0 = 2
    | otherwise = 1
