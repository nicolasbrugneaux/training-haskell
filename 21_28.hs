-- https://wiki.haskell.org/99_questions/21_to_28

module Problems21_28
( insertAt
, range
, rnd_select
, diff_select
, rnd_permu
, combinations
, myGroup
, lsort
, lfsort
) where

import System.Random
import Data.List
import Data.Function

-- can't seem to import Problems11_20.removeAt so i just copy it here
-- problem 20
removeAt :: Int -> [a] -> [a]
removeAt 1 (x:xs) = xs
removeAt n (x:xs) = x : removeAt (n - 1) xs

-- problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = take (i - 1) xs ++ [x] ++ drop (i - 1) xs

-- problem 22
range :: Int -> Int -> [Int]
range begin end = [begin..end]

-- problem 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select _  0 = return []
rnd_select [] _ = return []
rnd_select xs count = do
    r <- randomRIO (0, (length xs) - 1)
    rest <- rnd_select (removeAt (r + 1) xs)  (count - 1)
    return ((xs !! r) : rest)


-- problem 24
diff_select :: Int -> Int -> IO [Int]
diff_select n m = rnd_select [1..m] n

-- problem 25
rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)

takeIO :: Int -> IO [a] -> IO [a]
takeIO n xs = do
    _xs <- xs
    return $ take n _xs

-- problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs) - 1]
                                  , x <- combinations (n - 1) (drop (i + 1) xs)
                    ]

-- problem 27
combination :: Int -> [a] -> [([a], [a])]
combination 0 xs = [([], xs)]
combination n [] = []
combination n (x:xs) = ts ++ ds where
    ts = [ (x:ys, zs) | (ys, zs) <- combination (n - 1) xs ]
    ds = [ (ys, x:zs) | (ys, zs) <- combination n xs ]

myGroup :: [Int] -> [a] -> [[[a]]]
myGroup [] xs = [[]]
myGroup (g:gs) xs = concatMap grouper $ combination g xs where
    grouper (as, bs) = map (as:) (myGroup gs bs)

-- problem 28
lsort :: [[a]] -> [[a]]
-- lsort = sortBy (\xs ys -> compare (length xs) (length ys))
lsort = sortBy (compare `on` length)

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy ((==) `on` length) . lsort
