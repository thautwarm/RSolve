module RSolve.Options.Core where
import RSolve.BrMonad
import RSolve.Infr
import RSolve.Logic
import Control.Monad
import Control.Applicative
import Prelude hiding (not, or, and)
import qualified Data.Set  as S
import qualified Data.Map  as M
import qualified Data.List as L

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
  prune a = pruneSol a >>= return . Var . fst
  unify l r =
    pruneSol l >>= \(lFrom, lxsm) ->
    pruneSol r >>= \(rFrom, rxsm) ->
    case (lxsm, rxsm) of
       (Nothing,   _)   -> update lFrom (Var rFrom)
       (Just lxs,  _) | S.null lxs -> empty
       (_, Just rxs)  | S.null rxs -> empty
       (Just _,    Nothing )   -> unify r l
       (Just lxs,  Just rxs)   ->
        let xs = S.intersection lxs rxs in
        if S.null xs
        then empty
        else do
        new <- store $ Sol xs
        update lFrom new >> update rFrom new

  complement l r = do
    (l, Just lxs) <- pruneSol l
    (r, Just rxs) <- pruneSol r
    case (S.size lxs, S.size rxs) of
      (1, 1)   | lxs == rxs -> empty
      (1, 1)   | lxs /= rxs -> return ()
      (nl, nr) | nl < nr    -> complement (Var r) (Var l)
      (nl, nr) | nl >= nr   -> do
         let
           x:xs = L.map f . S.toList $ rxs
           f :: Option -> Br (LState Term) ()
           f re =
            let lnew_set = S.delete re lxs
            in
            if S.null lnew_set
            then empty
            else do
              lnew <- store . Sol $ lnew_set
              rnew <- store . Sol . S.singleton $ re
              update l lnew >> update r rnew
         L.foldl (<|>) x xs

instance EnumSet Term where
  toEnumerable = do
    st <- getBy $ storage . allocator
    M.foldlWithKey f (return ()) st
    where
      f :: Br (LState Term) () -> Addr -> Term -> Br (LState Term) ()
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
                  _  -> s <|> g xs
            in a >> g lst

