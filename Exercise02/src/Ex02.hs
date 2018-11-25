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

-- ins _ _ _  = error "ins NYI"


-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => Tree k d -> k -> m d

lkp Nil key = fail("nil")
--lkp Nil key
-- = Nil

lkp (Leaf k d) key
    = if k == key
        then return d
      else fail("no match")

lkp (Br brl brr k d) key
    = if k == key
         then return d
      else if k > key
              then lkp brl key
           else if k < key
                   then lkp brr key
                else fail("no match")
--lkp (Br brl brr k d) key
-- k == key = d
-- k > key = lkp brl key
-- k < key = lkp brr key

lkp _ _ = error "lkp NYI"

-- Part 3 : Instance of Num for Expr

{-
  Fix the following instance for Num of Expr so all tests pass

  Note that the tests expect simplification to be done
  only when *all* Expr arguments are of the form Val v

  Hint 1 :  implementing fromInteger *first* is recommended!
  Hint 2 :  remember that Double is already an instance of Num
-}

instance Num Expr where
  e1 + e2 = (Add e1 e2)
  e1 - e2 = Sub e1 e2
  e1 * e2 = Mul e1 e2
  negate e = Sub (Val(0.0)) e
  abs e = Abs e
  signum e = Sign e
  fromInteger i = Val(fromIntegral(i))
