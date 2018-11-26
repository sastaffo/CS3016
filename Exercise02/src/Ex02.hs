{- sastaffo Sarah Stafford-Langan -}
module Ex02 where

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype
data Tree k d
  = Br (Tree k d) (Tree k d) k d
  | Leaf k d
  | Nil
  deriving (Eq, Show)

type IntFun = Tree Int Int -- binary tree with integer keys and data

data Expr
  = Val Double
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Abs Expr
  | Sign Expr
   deriving (Eq, Show)



-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> Tree k d -> Tree k d

ins newkey newdata Nil  = Leaf newkey newdata

ins newkey newdata (Leaf k d)
    | k == newkey = Leaf k newdata
    | k >  newkey = Br (Leaf newkey newdata) Nil k d
    | k <  newkey = Br Nil (Leaf newkey newdata) k d

ins newkey newdata (Br brl brr k d)
    | k == newkey = Br brl brr k newdata
    | k >  newkey = Br (ins newkey newdata brl) brr k d
    | k <  newkey = Br brl (ins newkey newdata brr) k d


-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => Tree k d -> k -> m d

lkp Nil key = fail("nil")

lkp (Leaf k d) key
    = if k == key
        then return d
      else fail("no match")

lkp (Br brl brr k d) key
    = if k == key
         then return d
      else if k > key
              then lkp brl key
           else lkp brr key -- k < key


-- Part 3 : Instance of Num for Expr

{-
  Fix the following instance for Num of Expr so all tests pass

  Note that the tests expect simplification to be done
  only when *all* Expr arguments are of the form Val v

  Hint 1 :  implementing fromInteger *first* is recommended!
  Hint 2 :  remember that Double is already an instance of Num
-}

instance Num Expr where
  e1 + e2 = addex e1 e2
  e1 - e2 = subex e1 e2
  e1 * e2 = mulex e1 e2
  negate e = negex e
  abs e = absex e
  signum e = sigex e
  fromInteger i = Val(fromIntegral(i))

addex :: Expr -> Expr -> Expr
addex (Val n1) (Val n2) = Val (n1+n2)
addex e1 e2 = Add e1 e2

subex :: Expr -> Expr -> Expr
subex (Val n1) (Val n2) = Val (n1-n2)
subex e1 e2 = Sub e1 e2

mulex :: Expr -> Expr -> Expr
mulex (Val n1) (Val n2) = Val (n1*n2)
mulex e1 e2 = Mul e1 e2

negex :: Expr -> Expr
negex (Val n) = Val (0-n)
negex e = Sub (Val 0.0) e

absex :: Expr -> Expr
absex (Val n) = Val (abs n)
absex e = Abs e

sigex :: Expr -> Expr
sigex (Val n)
    = if n > 0
        then (Val 1.0)
        else if n < 0
            then (Val (-1.0))
            else (Val 0.0)
sigex e = Sign e

-- end
