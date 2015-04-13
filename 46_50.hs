module Main
( table
, gray
) where


_not :: Bool -> Bool
_not True  = False
_not False = True

_and, _or, _nand, _nor, _xor, _impl, _equ :: Bool -> Bool -> Bool

_and True True = True
_and _    _    = False

_or False False = False
_or _     _     = True

_nand = _and . _not

_nor = _or . _not

_xor = _equ . _not

_impl a b = _or (_not a) b

_equ a b = a == b

-- problem 46
table :: (Bool -> Bool -> Bool) -> IO()
table f = mapM_ putStrLn [ show a ++ "\t" ++ show b ++ "\t" ++ show (f a b)
    | a <- [True, False], b <- [True, False] ]

-- problem 47
-- problem 48

-- problem 49
gray :: Integral a => a  -> [String]
gray 0 = [""]
gray n = let xs = gray (n-1) in map ('0':) xs ++ map ('1':) (reverse xs)

-- problem 50 wtf??
