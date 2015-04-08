-- https://wiki.haskell.org/99_questions/1_to_10

module Problems1_10
( myLast
, myButLast
, elementAt
, myLength
, myReverse
, isPalindrome
, myFlatten
, compress
, pack
, encode
) where

-- problem 1
myLast :: [a] -> a
myLast [] = error "No last for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a
myButLast [] = error "No but last for empty lists"
myButLast [x] = error "No but last for list of length 1"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- problem 3
-- !! operator gives the index 0-based, the problem wants it 1-based
elementAt :: [a] -> Int -> a
elementAt x i = x !! (i - 1)

-- problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- probleme 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (myReverse xs)

-- problem 7
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

-- problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

-- problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)


-- problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack
