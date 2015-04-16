module Problems54_60
( cbalTree
, mirror
, symmetric
, construct
, symCbalTrees
, hbalTree
, hbalTreeNodes
) where

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- problem 54 is not a problem apparently :D

-- problem 55
cbalTree :: Int -> [(Tree Char)]
cbalTree 0 = [Empty]
cbalTree 1 = [leaf 'x']
cbalTree n =
    if n `mod` 2 == 1 then
        [ Branch 'x' l r | l <- cbalTree ((n - 1) `div` 2),
                           r <- cbalTree ((n - 1) `div` 2 ) ]
    else
        concat [ [Branch 'x' l r, Branch 'x' r l] | l <- cbalTree ((n - 1) `div` 2),
                                                    r <- cbalTree (n `div` 2) ]

-- problem 56
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

-- problem 57
add :: Ord a => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of
    LT -> Branch y (add x l) r
    GT -> Branch y l (add x r)
    EQ -> t

construct :: [Int] -> (Tree Int)
construct xs = foldl (flip add) Empty xs

-- problem 58
symCbalTrees :: Int -> [(Tree Char)]
symCbalTrees = filter symmetric . cbalTree

-- problem 59
hbalTree :: a -> Int ->[ Tree a]
hbalTree x = map fst . hbalTree where
    hbalTree 0 = [(Empty, 0)]
    hbalTree 1 = [(Branch x Empty Empty, 1)]
    hbalTree n = let t = hbalTree (n - 2) ++ hbalTree (n - 1) in
        [(Branch x lb rb, h) |
            (lb, lh) <- t,
            (rb, rh) <- t,
            let h = 1 + max lh rh,
            h == n
        ]

-- problem 60
maxNodes :: Int -> Int
maxNodes h = 2^h - 1

minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 $ fromIntegral (n + 1)

minNodes :: Int -> Int
minNodes h = fibs !! (h + 2) - 1

maxHeight :: Int -> Int
maxHeight n = length (takeWhile (<= n + 1) fibs) - 3

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = [ t | h <- [minHeight n .. maxHeight n], t <- baltree h n] where
    baltree 0 n = [Empty]
    baltree 1 n = [(Branch x Empty Empty)]
    baltree h n = [ Branch x l r |
        (hl, hr) <- [(h - 2, h - 1), (h - 1, h - 1), (h - 1, h -2)],
        let min_nl = max (minNodes hl) (n - 1 - maxNodes hr),
        let max_nl = min (maxNodes hl) (n - 1 - minNodes hr),
        nl <- [min_nl .. max_nl],
        let nr = n - 1 - nl,
        l <- baltree hl nl,
        r <- baltree hr nr ]
