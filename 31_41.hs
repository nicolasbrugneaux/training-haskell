-- https://wiki.haskell.org/99_questions/31_to_41

module Problems31_41
( isPrime
, myGcd
, coprime
, totient
, primeFactors
, primeFactorsTuples
, phi
, primesR
, goldbach
, goldbachList
) where

import Data.List

-- problem 31
primes :: [Int]
primes = nextPrime [2..]
    where nextPrime (p:xs) = p:nextPrime [x | x <- xs, x `mod` p > 0]

isPrime :: Int -> Bool
isPrime n = head (dropWhile (< n) primes) == n

-- problem 32
myGcd :: Int -> Int -> Int
myGcd a b
    | b == 0 = abs a
    | otherwise = myGcd b (a `mod` b)

-- problem 33
coprime :: Int -> Int -> Bool
coprime a b  = 1 == myGcd a b

-- problem 34
totient :: Int -> Int
totient 1 = 1
totient m = length [ x | x <- [1..(m - 1)], coprime x m ]

-- problem 35
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n
    | factors == [] = [n]
    | otherwise = factors ++ primeFactors (n `div` (head factors))
    where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2..(n - 1)]

-- problem 36
primeFactorsTuples :: Int -> [(Int, Int)]
primeFactorsTuples = map encode . group . primeFactors
    where encode xs = (head xs, length xs)

-- problem 37
phi :: Int -> Int
-- phi m = foldl (\acc x -> (*) acc (multiplyPrimeTuple x)) 1 (primeFactorsTuples m) where
phi m = foldl (flip ((*) . multiplyPrimeTuple)) 1 (primeFactorsTuples m) where
    multiplyPrimeTuple :: (Int, Int) -> Int
    multiplyPrimeTuple (p, f) = (*) (p - 1) ((^) p (f - 1))

-- problem 38
-- just check the speed, phi is crazy faster than totient

-- problem 39
primesR :: Int -> Int -> [Int]
primesR n m = filter isPrime [n..m]

-- problem 40
goldbach :: Int -> (Int, Int)
goldbach n = head [ (x, y) | x <- _primes, y <- _primes, x + y == n ] where
    _primes = primesR 2 (n - 2)

-- problem 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList n m = map goldbach (dropWhile (< 4) (filter even [n..m]))
