module Cis194_homework3 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st key value = \k -> if k == key then value else st k

empty :: State
empty = \_-> 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var s) = st s
evalE st (Val i) = i
evalE st (Op leftHand bop rightHand) = case bop of
    Plus   -> left + right
    Minus  -> left - right
    Times  -> left * right
    Divide -> left `div` right
    Gt     -> fromBool (left > right)
    Ge     -> fromBool (left >= right)
    Lt     -> fromBool (left < right)
    Le     -> fromBool (left <= right)
    Eql    -> fromBool (left == right)
    where
        left  = evalE st leftHand
        right = evalE st rightHand
        fromBool True  = 1
        fromBool False = 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign str expr)              = DAssign str expr
desugar (Incr str)                     = DAssign str $ Op (Var str) Plus (Val 1)
desugar (If expr thenSt  elseSt)       = DIf expr (desugar thenSt) (desugar elseSt)
desugar (While expr st)                = DWhile expr (desugar st)
desugar (For initSt expr incSt doSt)   = DSequence before (DWhile expr doOperation)
    where before      = desugar initSt
          doOperation = desugar (Sequence doSt incSt)
desugar (Sequence left right)          = DSequence (desugar left) (desugar right)
desugar (Skip)                         = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign str expr) =  extend st str $ evalE st expr
evalSimple st (DIf expr thenSt elseSt) = if condition then thenRes else elseRes
    where condition = 0 /= (evalE st expr)
          thenRes   = evalSimple st thenSt
          elseRes   = evalSimple st elseSt
evalSimple st (DWhile expr statement)
    | bool      = evalSimple newSt (DWhile expr statement)
    | otherwise = st
    where bool  = evalE st expr /= 0
          newSt = evalSimple st statement
evalSimple st (DSequence left right) = evalSimple (evalSimple st left) right
evalSimple st DSkip = st


run :: State -> Statement -> State
run st statement = evalSimple st $ desugar statement

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
