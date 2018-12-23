{-# LANGUAGE GADTs  #-}
module Rcheck.Logic where
import Rcheck.BrMonad
import Rcheck.Solver
import Data.List (nub)

data Cond a where
   Unify :: Unify a => a -> a -> Cond a
   Not   :: Cond a -> Cond a
   Pred  :: Br (LState a) Bool -> Cond a

   Or    :: Cond a -> Cond a -> Cond a
   And   :: Cond a -> Cond a -> Cond a
   Imply :: Cond a -> Cond a -> Cond a

solve :: Cond a -> Br (LState a) ()
solve (Unify l r) = unify l r

solve (Or l r)    = solve l `union` solve r

solve (And l r)   =
  solve l >> solve r

solve (Imply l r) =
  (solve l >> solve r) `union` solve (Not l)

solve (Pred c)    = do
  cs <- getBy constrains
  putBy $ constrains' (c:cs)

solve (Not emmm)  =
  case emmm of
    Pred  c    -> solve $ Pred (c >>= return . not)
    Not emmm   -> solve emmm
    Or     l r -> solve $ And (Not l)(Not r)
    And    l r -> solve $ Or  (Not l)(Not r)
    Imply  l r -> solve $ Imply l (Not r)
    Unify  l r -> do
     l <- prune l
     r <- prune r
     negUnify l r

solveNeg :: Complement a => Br (LState a) ()
solveNeg = do
  negs <- getBy negPairs
  negs <- pruneTuples negs
  let negs' = nub (negs)
  solveNeg' negs'
  where
    pruneTuples [] = return []
    pruneTuples ((a, b):xs) = do
      a <- prune (mkRef a)
      b <- prune (mkRef b)
      xs' <- pruneTuples xs
      let x' = if a > b then (a, b) else (b, a)
      return $ x' : xs
    solveNeg' [] = return ()
    solveNeg' ((a,b):xs) =
      (a `complement` b) >> solveNeg' xs

solvePred :: EnumSet a => Br (LState a) ()
solvePred = do
  _ <- toEnumerable
  cs <- getBy constrains
  checkPredicate cs
  where
    checkPredicate [] = return ()
    checkPredicate (x:xs) = do
      x <- x
      if x then checkPredicate xs
      else reset

require :: Unify a => a -> Br (LState a) a
require a = do
  a <- prune a
  load a



