{-# LANGUAGE GADTs  #-}
module Main where
import Prelude hiding (and, or, not)
import Control.Monad
import Data.List (concat, nub)
import Control.Monad.Trans
import qualified Prelude as Prel
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S

-- monad for branched computation
data Br s a = Br {runBr :: s -> [(a, s)]}

instance Functor (Br s) where
  fmap = liftM

instance Applicative (Br s) where
  pure = return
  (<*>)  = ap

instance Monad (Br s) where
  m >>= k =
    Br $ \s ->
       let xs = runBr m s
       in concat[ runBr (k a) s | (a, s) <- xs]
  return a = Br $ \s -> [(a, s)]

getBy f = Br $ \s -> [(f s, s)]
putBy f = Br $ \s -> [((), f s)]

union :: Br s a -> Br s a -> Br s a
union ma mb =
  Br $ \s ->
  runBr ma s ++ runBr mb s

reset :: Br s ()
reset = Br $ \s -> []

-- extract problem specific descriptions
data Option  = A | B | C | D
 deriving (Eq, Show, Ord, Enum)

data Term    = Var Int | Sol (S.Set Option)
 deriving (Eq, Show)

-- logic unification and storage infrastructure
class Reference a where
  isRef :: a  -> Maybe Int
  mkRef    :: Int -> a

class (Eq a, Reference a) => Unify a where
  prune       :: a -> Br (LState a) Int
  unify       :: a -> a -> Br (LState a) ()
  unifyNot    :: Int -> Int -> Br (LState a) ()
  toSingleton :: Br (LState a) ()

data Cond a where
   Unify :: Unify a => a -> a -> Cond a
   Not   :: Cond a -> Cond a
   Pred  :: Br (LState a) Bool -> Cond a

   Or    :: Cond a -> Cond a -> Cond a
   And   :: Cond a -> Cond a -> Cond a
   Imply :: Cond a -> Cond a -> Cond a

data Allocator a =
  Allocator { storage :: M.Map Int a
            , addr    :: Int }
  deriving (Show)

data LState a =
  LState { allocator  :: Allocator a
         , negPairs   :: [(Int, Int)]
         , constrains :: [Br (LState a) Bool]}

setAllocator st (LState _ negs cs)  = LState st negs cs
setNegPairs  negs (LState st _ cs)  = LState st negs cs
setConstrains cs (LState st negs _) = LState st negs cs

alloc :: Reference a => a -> Allocator a -> (Int, Allocator a)
alloc a (Allocator s c) =
  case isRef a of
    Just _ -> error "invalid allocation"
    _      -> (c, Allocator (M.insert c a s) (c + 1))

renew :: Reference a => Int -> a -> Allocator a -> Allocator a
renew addr o r@(Allocator s c) =
  case isRef o of
    Just addr' | addr' == addr -> r
    _ ->  Allocator (M.insert addr o s) c

-- store into state
store :: Reference a => Eq a => a -> Br (LState a) a
store a = do
  st <- getBy allocator
  let (n, st') = alloc a st
  _ <- putBy $ setAllocator st'
  return $ mkRef n

-- update state
update  :: Reference a => Int -> a -> Br (LState a) ()
update a b = do
  st <- getBy allocator
  putBy $ (setAllocator . renew a b $ st)

-- load from state
load :: Int -> Br (LState a) a
load addr =
  getBy allocator >>= return . (flip (M.!) $ addr) . storage

addNegPair :: Int -> Int -> Br (LState a) ()
addNegPair a b = do
  negs <- getBy negPairs
  if check negs then
     putBy $ setNegPairs ((a, b) : negs)
  else return ()
  where
    check [] = True
    check ((a', b'):xs) =
      if (a', b') == (a, b) || (a', b') == (b, a)
      then False
      else check xs

solve :: Cond a -> Br (LState a) ()
solve (Unify l r)    = unify l r

solve (Or l r)    =
  solve l `union` solve r

solve (And l r)   =
  solve l >> solve r

solve (Imply l r) =
  (solve l >> solve r) `union` solve (Not l)

solve (Pred c) = do
  cs <- getBy constrains
  putBy $ setConstrains (c:cs)

solve (Not emmm)  =
  case emmm of
    Pred  c    -> solve $ Pred (c >>= return . Prel.not)
    Not emmm   -> solve emmm
    Or     l r -> solve $ And (Not l)(Not r)
    And    l r -> solve $ Or  (Not l)(Not r)
    Imply  l r -> solve $ Imply l (Not r)
    Unify  l r -> do
     l <- prune l
     r <- prune r
     addNegPair l r

finalize :: (Show a, Unify a) => Br (LState a) ()
finalize = do
  negs <- getBy negPairs
  negs <- pruneTuples negs
  let negs' = nub (negs)
  _  <- solveNeq negs'
  _  <- toSingleton
  cs <- getBy constrains
  checkPredicate cs
  where
    checkPredicate [] = return ()
    checkPredicate (x:xs) = do
      x <- x
      if x then checkPredicate xs
      else reset
    pruneTuples [] = return []
    pruneTuples ((a, b):xs) = do
      a <- prune (mkRef a)
      b <- prune (mkRef b)
      xs' <- pruneTuples xs
      let x' = if a > b then (a, b) else (b, a)
      return $ x' : xs
    solveNeq [] = return ()
    solveNeq ((a,b):xs) =
      (a `unifyNot` b) >> solveNeq xs

require :: Unify a => a -> Br (LState a) a
require a = do
  a <- prune a
  load a

-- solve the negations finally
instance Reference Term where
  isRef (Var addr) = Just addr
  isRef _ = Nothing
  mkRef a = Var a

instance Unify Term where
  toSingleton = do
    st <- getBy $ storage . allocator
    M.foldlWithKey f (return ()) st
    where
      f :: Br (LState Term) () -> Int -> Term -> Br (LState Term) ()
      f a k b =
        case b of
          Var _ -> a
          Sol set ->
            let
              lst = S.toList set
              g :: [Option] -> Br (LState Term) ()
              g [] = error "unexpected"
              g (x:xs) = do
                x <- store . Sol . S.singleton $ x
                let s = update k x
                case xs of
                  [] -> s
                  _  -> s `union` g xs
            in a >> g lst
  prune a = pruneSol a >>= return.fst
  unifyNot l r = do
    (l, Just lxs) <- pruneSol (Var l)
    (r, Just rxs) <- pruneSol (Var r)
    case (S.size lxs, S.size rxs) of
      (1, 1)   | lxs == rxs -> reset
      (1, 1)   | lxs /= rxs -> return ()
      (nl, nr) | nl < nr    -> unifyNot r l
      (nl, nr) | nl >= nr   -> do
         let
           x:xs = L.map f . S.toList $ rxs
           f :: Option -> Br (LState Term) ()
           f re =
            let lnew_set = S.delete re lxs
            in
            if S.null lnew_set
            then reset
            else do
              lnew <- store . Sol $ lnew_set
              rnew <- store . Sol . S.singleton $ re
              update l lnew >> update r rnew
         L.foldl union x xs
  unify l r =
    pruneSol l >>= \(lFrom, lxsm) ->
    pruneSol r >>= \(rFrom, rxsm) ->
    case (lxsm, rxsm) of
       (Nothing,   Nothing )   -> update lFrom (Var rFrom)
       (Just lxs,  _) | S.null lxs -> reset
       (_, Just rxs)  | S.null rxs -> reset
       (Just _,    Nothing )   -> unify r l
       (Nothing,   Just _  )   -> update lFrom (Var rFrom)
       (Just lxs,  Just rxs)   -> do
        let xs = S.intersection lxs rxs
        if S.null xs
        then reset
        else do
        new <- store $ Sol xs
        update lFrom new >> update rFrom new

pruneSol :: Term -> Br (LState Term) (Int, Maybe (S.Set Option))
pruneSol (Var addr) = do
  t <- load addr
  case t of
    Var addr' -> do
      r @ (addrLast, _) <- pruneSol t
      update addr (Var addrLast) >> return r
    Sol lxs    ->
      return (addr, Just lxs)
      -- if S.null lxs then error "emmm"
      -- else return (addr, Just lxs)

pruneSol r @ (Sol xs) =
  store r >>= \(Var addr) ->
  return (addr, Just xs)

sol = Sol . S.fromList
total = [A, B, C, D]
toSol a = do
  (_, Just b) <- pruneSol a
  if S.size b /= 1 then error $ show b
  else return $ S.elemAt 0 b
eq a b  = Unify a b
neq a b = Not $ a `eq` b
not = Not
and = And
or  = Or
(|-) a b = Imply a b

(==>) :: Option -> (Cond Term) -> Term -> (Cond Term)
(==>) a b c = c `eq` sol [a] `and` b

(|||)   :: (Term -> (Cond Term)) -> (Term -> Cond Term) -> (Term -> Cond Term)
a ||| b = \t -> a t `or` b t

for :: Term -> (Term -> Cond Term) -> Br (LState Term) ()
for a f = solve $ f a

infixr 7 `eq`, `neq`
infixr 5 `or`
infixr 6 `and`, |-
infixr 4 ==>
infixr 3 |||

test = do
  _1 <- store $ sol total
  _2 <- store $ sol total
  _3 <- store $ sol total
  _4 <- store $ sol total
  _5 <- store $ sol total
  _6 <- store $ sol total
  _7 <- store $ sol total
  _8 <- store $ sol total
  _9 <- store $ sol total
  _10 <- store $ sol total
  _ <- for _2 $
    A ==> _5 `eq` sol [C] |||
    B ==> _5 `eq` sol [D] |||
    C ==> _5 `eq` sol [A] |||
    D ==> _5 `eq` sol [B]
  _  <- for _3 $
    let
       diff3 :: [Term] -> Term -> Cond Term
       diff3 lst a =
         let conds = [a `neq` e | e <- L.delete a lst]
         in case conds of
                []   -> error "emmm"
                x:xs -> L.foldl and x xs
       f = diff3 [_3, _6, _2, _4]
    in A ==> f _3 |||
       B ==> f _6 |||
       C ==> f _2 |||
       D ==> f _4
  _ <- for _4 $
     A ==> _1 `eq` _5 |||
     B ==> _2 `eq` _7 |||
     C ==> _1 `eq` _9 |||
     D ==> _6 `eq` _10
  _ <- for _5 $
     A ==> _5 `eq` _8 |||
     B ==> _5 `eq` _4 |||
     C ==> _5 `eq` _9 |||
     D ==> _5 `eq` _7
  _ <- for _6 $
     A ==> _2 `eq` _8 `and` _4 `eq` _8   |||
     B ==> _1 `eq` _8 `and`  _6 `eq` _8  |||
     C ==> _3 `eq` _8 `and`  _10 `eq` _8 |||
     D ==> _5 `eq` _8 `and` _9 `eq` _8
  let
    solution = do
      mapM toSol [_1, _2, _3, _4, _5, _6, _7, _8, _9, _10]
    count :: Br (LState Term) (M.Map Option Int)
    count = do
      solution <- solution
      return . countImpl $ solution
      where
        countImpl :: [Option] -> M.Map Option Int
        countImpl [] = M.empty
        countImpl (x:xs) = M.alter f x $ countImpl xs
        f Nothing = Just 1
        f (Just a) = Just $ a + 1
    msearch cond = do
      count <- count
      return $ M.foldlWithKey f [] count
      where
        f [] k v = [(k, v)]
        f r@((k', v'):_) k v =
         case compare v v' of
           a | a == cond  -> [(k, v)]
           EQ -> (k, v) : r
           _ -> r
    msearchNSuite :: (Int -> Int -> Bool) -> Option -> Br (LState Term) (Maybe Int)
    msearchNSuite cond opt = do
      count <- count
      case M.lookup opt count of
        Nothing -> do

          return (Just 0)
        Just n  ->
          let
            f Nothing k v = Nothing
            f r@(Just a) k v =
              if cond v n then Nothing
              else r
          in return $ M.foldlWithKey f (Just n) count
  _ <- for _7 $
     let minIs a =
           let m = do
                 lst <- msearch LT
                 return $ L.all (\(k, v) -> k == a) lst
           in Pred m
     in A ==> minIs C |||
        B ==> minIs B |||
        C ==> minIs A |||
        D ==> minIs D
  _ <- for _8 $
     let
       notAdjacent a b = do
         a <- toSol a
         b <- toSol b
         let sep = (fromEnum a - fromEnum b)
         return $ abs(sep) > 1
     in A ==> Pred (notAdjacent _1 _7)  |||
        B ==> Pred (notAdjacent _1 _5)  |||
        C ==> Pred (notAdjacent _1 _2)  |||
        D ==> Pred (notAdjacent _1 _10)
  _ <- for _9 $
     let
       f x =
        let a = _1 `eq` _6 in
        let b = x  `eq` _5 in
        not a `and` b `or` not b `and` a
     in A ==> f _6  |||
        B ==> f _10 |||
        C ==> f _2  |||
        D ==> f _9
  _ <- for _10 $
    let
      by a =
        Pred m
        where m = do
               (_, minCount):_ <- msearch LT
               (_, maxCount):_ <- msearch GT
               return $ maxCount - minCount == a
    in A ==> by 3 |||
       B ==> by 2 |||
       C ==> by 4 |||
       D ==> by 1
  _ <- finalize
  mapM require [_1, _2, _3, _4, _5, _6, _7, _8, _9, _10]

test2 = do
  a <- store $ sol [A, B, C]
  b <- store $ sol [B, C, D]
  c <- store $ sol [B]
  _ <- solve $ Unify a b
  _ <- solve $ Not (Unify a c)
  _ <- finalize
  mapM require [a, b, c]
format :: [String] -> [[Term]] -> IO ()
format names xs =
  let
    formatCell :: (String, Term) -> IO()
    formatCell (a, b) = putStrLn $ show a ++ " : " ++ show b
    formatLine :: [(String, Term)] -> IO()
    formatLine xs = do
      _ <- putStrLn "===="
      forM_ xs formatCell
    formatLines xs =
      forM_ xs $ \line -> formatLine $ L.zip names line
  in formatLines xs
emptyAllocator = Allocator (M.empty) 0
emptyLState    = LState emptyAllocator [] []

test3 :: Br Int ()
test3  = do
  a <- getBy id
  if a > 0 then putBy $ \i -> i + 1
  else reset


--    s1
--  /      \
-- s21  or   s22
-- | \      | \
-- s31 s32  [s33, s34, s35]
test4 :: Br Int ()
test4  = do
  a <- getBy id
  if a > 0 then putBy $ \i -> i + 2
  else reset

thres :: Int -> Br Int ()
thres a = do
  s <- getBy id
  if s < a then reset
  else return ()
main = do
  putStrLn . show $ runBr (union test3 test4 >> thres 3) 1
   -- format ["a", "b", "c"] . nub . L.map fst
   -- $ runBr test2 emptyLState
