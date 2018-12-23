module Main where
import RSolve.BrMonad
import RSolve.Solver
import RSolve.Logic
import Control.Monad
import Prelude hiding (not, or, and)
import qualified Data.Set  as S
import qualified Data.Map  as Ms
import qualified Data.List as L

nub = L.nub

data Option  = A | B | C | D
 deriving (Eq, Show, Ord, Enum)

data Term    = Var Int | Sol (S.Set Option)
 deriving (Eq, Show)

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


instance Reference Term where
  isRef (Var addr) = Just addr
  isRef _ = Nothing
  mkRef a = Var a



instance Unify Term where
  prune a = pruneSol a >>= return.fst
  unify l r =
    pruneSol l >>= \(lFrom, lxsm) ->
    pruneSol r >>= \(rFrom, rxsm) ->
    case (lxsm, rxsm) of
       (Nothing,   Nothing )   -> update lFrom (Var rFrom)
       (Just lxs,  _) | S.null lxs -> reset
       (_, Just rxs)  | S.null rxs -> reset
       (Just _,    Nothing )   -> unify r l
       (Nothing,   Just _  )   -> update lFrom (Var rFrom)
       (Just lxs,  Just rxs)   ->
        let xs = S.intersection lxs rxs in
        if S.null xs
        then reset
        else do
        new <- store $ Sol xs
        update lFrom new >> update rFrom new

instance Complement Term where
  complement l r = do
    (l, Just lxs) <- pruneSol (Var l)
    (r, Just rxs) <- pruneSol (Var r)
    case (S.size lxs, S.size rxs) of
      (1, 1)   | lxs == rxs -> reset
      (1, 1)   | lxs /= rxs -> return ()
      (nl, nr) | nl < nr    -> complement r l
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

instance EnumSet Term where
  toEnumerable = do
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
  _ <- solveNeg
  _ <- solvePred
  mapM require [_1, _2, _3, _4, _5, _6, _7, _8, _9, _10]

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


main = do
   format [show i | i <- [1..10]] . nub . L.map fst
   $ runBr test emptyLState
