module Rcheck.Solver where
import Rcheck.BrMonad

import qualified Data.Set  as S
import qualified Data.Map  as M
import qualified Data.List as L

type Addr = Int
class Reference a where
  isRef :: a  -> Maybe Addr
  mkRef :: Addr -> a

class (Eq a, Reference a) => Unify a where
  prune  :: a -> Br (LState a) Addr
  unify  :: a -> a -> Br (LState a) ()

class Unify a => Complement a where
  complement :: Addr -> Addr -> Br (LState a) ()

class EnumSet a where
  toEnumerable :: Br (LState a) ()


data Allocator a =
  Allocator { storage :: M.Map Int a
            , addr    :: Int }
  deriving (Show)


data LState a =
   LState { allocator  :: Allocator a
          , negPairs   :: [(Int, Int)]
          , constrains :: [Br (LState a) Bool] }

allocator'  st   (LState _ negs cs) = LState st negs cs
negPairs'   negs (LState st _   cs) = LState st negs cs
constrains' cs   (LState st negs _) = LState st negs cs

alloc :: Reference a => a -> Allocator a -> (Int, Allocator a)
alloc a (Allocator s c) =
  case isRef a of
    Just _ -> error "Allocation requires a value instead of a reference."
    _      -> (c, Allocator (M.insert c a s) (c + 1))

renew :: Reference a => Int -> a -> Allocator a -> Allocator a
renew addr obj r@(Allocator s c) =
  case isRef obj of
    Just addr' | addr' == addr -> r -- avoid recursive definition
    _ ->  Allocator (M.insert addr obj s) c

store :: (Reference a, Eq a) => a -> Br (LState a) a
store a = do
  st <- getBy allocator
  let (n, st') = alloc a st
  _ <- putBy $ allocator' st'
  return $ mkRef n


-- update state
update  :: Reference a => Int -> a -> Br (LState a) ()
update addr obj = do
  st <- getBy allocator
  putBy $ (allocator' . renew addr obj $ st)


load :: Int -> Br (LState a) a
load addr =
  getBy allocator >>= return . (flip (M.!) $ addr) . storage


negUnify :: Int -> Int -> Br (LState a) ()
negUnify a b = do
  negs <- getBy negPairs
  if check negs then
     putBy $ negPairs' ((a, b) : negs)
  else return ()
  where
    check [] = True
    check ((a', b'):xs) =
      if (a', b') == (a, b) || (a', b') == (b, a)
      then False
      else check xs
