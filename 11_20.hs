-- https://wiki.haskell.org/99_questions/11_to_20

module Problems11_20
( encodeModified
, decodeModified
, encodeDirect
, duplicate
, repli
, dropEvery
, split
, slice
, rotate
, removeAt
) where

import qualified Problems1_10

-- problem 11
data SingleOrMultiple a = Multiple Int a | Single a
    deriving (Show)
encodeModified :: Eq a => [a] -> [SingleOrMultiple a]
encodeModified = map encoder . Problems1_10.encode where
    encoder (1, x) = Single x
    encoder (n, x) = Multiple n x

-- problem 12
decodeModified :: [SingleOrMultiple a] -> [a]
decodeModified = concatMap decoder where
    decoder (Single x) = [x]
    decoder (Multiple n x) = replicate n x

-- problem 13
encodeDirect :: Eq a => [a] -> [SingleOrMultiple a]
encodeDirect [] = []
encodeDirect (x:xs)
    | count == 1 = (Single x) : (encodeDirect xs)
    | otherwise = (Multiple count x) : (encodeDirect rest)
    where
        (matched, rest) = span (== x) xs
        count = 1 + (length matched)

-- problem 14
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

-- problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap replicator xs where
    replicator = take n . repeat

-- problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = _drop xs 1 where
    _drop :: [a] -> Int -> [a]
    _drop [] _ = []
    _drop (x:xs) i
        | i == n = _drop xs 1
        | i /= n = x:_drop xs (i + 1)

-- problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs begin end = if begin > end
                     then []
                     else (take (end - begin + 1) (drop (begin - 1) xs))

-- problem 19
rotate :: [a] -> Int -> [a]
rotate xs n = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs

-- problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (front, x:rest) where
    (front, rest) = removeAt (n - 1) xs
