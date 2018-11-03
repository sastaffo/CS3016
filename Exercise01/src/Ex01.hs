{- butrfeld Andrew Butterfield -}
module Ex01 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Eq k => Dict k d -> k -> Maybe d
find []             _                 =  Nothing
find ( (s,v) : ds ) name | name == s  =  Just v
                         | otherwise  =  find ds name

type EDict = Dict String Double

-- Part 1 : Evaluating Expressions -- (63 marks) -------------------------------

-- Implement the following function so all 'eval' tests pass.

-- eval should return Nothing if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.

eval :: EDict -> Expr -> Maybe Double
eval _ (Val x) = Just x
eval d (Var x) = find d x
eval d (Add e1 e2)    -- = evalOp (+) d e1 e2
    = case (eval d e1, eval d e2) of
        (Just m, Just n)    -> Just (m+n)
        _                   -> Nothing
eval d (Sub e1 e2)    -- = evalOp (-) d e1 e2
    = case (eval d e1, eval d e2) of
        (Just m, Just n)    -> Just (m-n)
        _                   -> Nothing
eval d (Mul e1 e2)    -- = evalOp (*) d e1 e2
    = case (eval d e1, eval d e2) of
        (Just m, Just n)    -> Just (m*n)
        _                   -> Nothing
eval d (Dvd e1 e2)
    = case (eval d e1, eval d e2) of
        (Just m, Just n)    -> if n==0.0 then Nothing else Just (m/n)
        _                   -> Nothing
eval d (Def x e1 e2)
    = case (eval d e1) of
        Nothing -> Nothing
        Just v  -> eval  (define d x v) e2
eval d e            = Nothing

-- Part 2 : Simplifying Expressions -- (57 marks) ------------------------------

-- Given the following code :

simp :: EDict -> Expr -> Expr
simp d (Var v)        =  simpVar d v
simp d (Add e1 e2)    =  simpAdd d   (simp d e1) (simp d e2)
simp d (Sub e1 e2)    =  simpSub d   (simp d e1) (simp d e2)
simp d (Mul e1 e2)    =  simpMul d   (simp d e1) (simp d e2)
simp d (Dvd e1 e2)    =  simpDvd d   (simp d e1) (simp d e2)
simp d (Def v e1 e2)  =  simpDef d v (simp d e1) (simp d e2)
simp _ e = e  -- simplest case, Val, needs no special treatment

-- Implement the following functions so all 'simp' tests pass.

  -- (1) see test scripts for most required properties
  -- (2) (Def v e1 e2) should simplify to just e2 in the following two cases:
    -- (2a) if there is mention of v in e2
    -- (2b) if any mention of v in e2 is inside another (Def v .. ..)

simpVar :: EDict -> Id -> Expr
simpVar d v = case (find d v) of
                (Just x)    -> Val(x)
                _           -> Var(v)

simpAdd :: EDict -> Expr -> Expr -> Expr
simpAdd d e1 e2 = let x = simp d e1
                      y = simp d e2
                  in case (x,y) of
                      (e, Val 0.0)  -> e
                      (Val 0.0, e)  -> e
                      _             -> case (eval d (Add x y)) of
                                            (Just p)    -> Val (p)
                                            _           -> Add x y

simpSub :: EDict -> Expr -> Expr -> Expr
simpSub d e1 e2 = let x = simp d e1
                      y = simp d e2
                  in case (x,y) of
                      (e, Val 0.0)  -> e
                      _             -> case (eval d (Sub x y)) of
                                            (Just p)    -> Val (p)
                                            _           -> Sub x y


simpMul :: EDict -> Expr -> Expr -> Expr
simpMul d e1 e2 = let x = simp d e1
                      y = simp d e2
                  in case (x,y) of
                      (e, Val 1.0)  -> e
                      (Val 1.0, e)  -> e
                      (e, Val 0.0)  -> Val(0)
                      (Val 0.0, e)  -> Val(0)
                      _             -> case (eval d (Mul x y)) of
                                            (Just p)    -> Val (p)
                                            _           -> Mul x y

simpDvd :: EDict -> Expr -> Expr -> Expr
simpDvd d e1 e2 = let x = simp d e1
                      y = simp d e2
                  in case (x,y) of
                      (e, Val 1.0)  -> e
                      (e, Val 0.0)  -> Dvd x y
                      _             -> case (eval d (Dvd x y)) of
                                            (Just p)    -> Val (p)
                                            _           -> Dvd x y

simpDef :: EDict -> Id -> Expr -> Expr -> Expr
simpDef d v e1 e2 = case (eval d e1) of
                     (Just x) -> let d1= (define d v x)
                                 in case (d1) of
                                     [(s, f)] -> case (eval d1 e2) of
                                                   (Just x3) -> Val(x3)
                                                   _         -> e2
                                     _        -> e2
                     _        -> e2

--
