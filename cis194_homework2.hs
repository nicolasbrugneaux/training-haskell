{-# OPTIONS_GHC -Wall #-}
module Cis194_homework2 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys) = acc + exactMatches xs ys
    where acc = if x == y
                then 1
                else 0

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (\x -> length $ filter (== x) xs) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ map (\(x, y) -> min x y) (zip (countColors xs) (countColors ys))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonExact
    where exact    = exactMatches secret guess
          nonExact = (matches secret guess) - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact nonExact) code = otherExact == exact && otherNonExact == nonExact
    where (Move _ otherExact otherNonExact) = getMove guess code

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move guesses = filter (\x -> isConsistent move x) guesses

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = concatMap (\x -> map (x:) $ allCodes (n - 1)) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = findMoves secret allMoves
    where allMoves = map (getMove secret) $ allCodes $ length secret
          findMoves :: Code -> [Move] -> [Move]
          findMoves _ [] = []
          findMoves secret (move@(Move _ exact _):xs)
              | exact == length secret = [move]
              | otherwise              = move : findMoves secret xs
-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
