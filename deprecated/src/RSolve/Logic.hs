{-# LANGUAGE GADTs #-}
module RSolve.Logic where
import RSolve.BrMonad
import RSolve.Infr
import Data.List (nub)
import Control.Applicative

data Cond a where
   Unify :: Unify a => a -> a -> Cond a
   Not   :: Cond a -> Cond a
   Pred  :: Br (LState a) Bool -> Cond a

   Or    :: Cond a -> Cond a -> Cond a
   And   :: Cond a -> Cond a -> Cond a
   Imply :: Cond a -> Cond a -> Cond a

solve :: Cond a -> Br (LState a) ()
solve (Unify l r) = do
  l <- prune l
  r <- prune r
  unify l r

solve (Or l r)    =
  solve l <|> solve (And (Not l) r)

solve (And l r)   =
  solve l >> solve r

solve (Imply l r) =
  solve (Not l) <|> solve r

solve (Pred c)    = do
  cs <- getBy constrains
  putBy $ constrains' (c:cs)

solve (Not emmm)  =
  case emmm of
    Pred  c    -> solve $ Pred (not <$> c)
    Not emmm   -> solve emmm
    Or     l r -> solve $ And (Not l)(Not r)
    And    l r -> solve $ Or  (Not l)(Not r)
    Imply  l r -> solve $ And  l (Not r)
    Unify  l r -> do
     l <- prune l
     r <- prune r
     negUnify l r

solveNeg :: Unify a => Br (LState a) ()
solveNeg = do
  negs <- getBy negPairs
  negs <- pruneTuples negs
  solveNeg' $ nub (negs)
  where
    pruneTuples [] = return []
    pruneTuples ((a, b):xs) = do
      a <- prune a
      b <- prune b
      xs' <- pruneTuples xs
      let
        process (Just a) (Just b) = x:xs
          where
            mkRef2 a b = (mkRef a, mkRef b)
            x = if a > b then mkRef2 a b else mkRef2 b a
        process _ _ = (a, b):xs'
      return $ process (isRef a) (isRef b)
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
      else empty

require :: Unify a => a -> Br (LState a) a
require a = do
  a <- prune a
  case isRef a of
      Just a -> load a
      _      -> return a


