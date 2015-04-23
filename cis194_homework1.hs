module Homework1
( validate
, hanoi
) where
import Data.List.Split

-- validating credit card numbers

double :: Int -> Int
double = (* 2)

toDigits :: Int -> [Int]
toDigits n = filter (> 0) (map (\x -> read [x] :: Int) (show n))

toDigitsRev :: Int -> [Int]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther = reverse . (zipWith (*) (cycle [1, 2])) . reverse

sumDigits :: [Int] -> Int
sumDigits = (foldl (+) 0) . (concatMap toDigits)

validate :: Int -> Bool
validate n = 0 == (sumDigits (doubleEveryOther (toDigits n))) `mod` 10


-- towers of hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, c)] ++ (hanoi (n - 1) b a c)
