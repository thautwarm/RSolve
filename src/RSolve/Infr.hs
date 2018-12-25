module RSolve.Infr where
import RSolve.BrMonad

import qualified Data.Set  as S
import qualified Data.Map  as M
import qualified Data.List as L

type Addr = Int
class Eq a => Reference a where
  -- reference can be stored in Map
  isRef :: a  -> Maybe Addr
  mkRef :: Addr -> a

class Reference a => Unify a where
  prune  :: a -> Br (LState a) a
  unify  :: a -> a -> Br (LState a) ()

class Unify a => Complement a where
  complement :: a -> a -> Br (LState a) ()

class EnumSet a where
  toEnumerable :: Br (LState a) ()


data Allocator a =
  Allocator { storage :: M.Map Addr a
            , addr    :: Addr }
  deriving (Show)


data LState a =
   LState { allocator  :: Allocator a
          , negPairs   :: [(a, a)]
          , constrains :: [Br (LState a) Bool] }

allocator'  st   (LState _ negs cs) = LState st negs cs
negPairs'   negs (LState st _   cs) = LState st negs cs
constrains' cs   (LState st negs _) = LState st negs cs


inc :: Reference a => Allocator a -> (Addr, Allocator a)
inc (Allocator s c) = (c, Allocator s $ c + 1)

alloc :: Reference a => a -> Allocator a -> (Addr, Allocator a)
alloc a (Allocator s c) = (c, Allocator (M.insert c a s) (c + 1))

renew :: Reference a => Addr -> a -> Allocator a -> Allocator a
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
update  :: Reference a => Addr -> a -> Br (LState a) ()
update addr obj = getBy allocator >>= putBy . allocator' . renew addr obj


load :: Addr -> Br (LState a) a
load addr =
  ((M.! addr) . storage) <$> getBy allocator


tryLoad :: Addr -> Br (LState a) (Maybe a)
tryLoad addr =
  (M.lookup addr . storage) <$> getBy allocator


-- for the system which take leverage of generics
new :: Reference a => Br (LState a) Addr
new = do
  st <- getBy allocator
  let (addr', st') = inc st
  _ <- putBy $ allocator' st'
  return addr'

negUnify :: Reference a => a -> a -> Br (LState a) ()
negUnify a b = do
  negs <- getBy negPairs
  if check negs then
     putBy $ negPairs' ((a, b) : negs)
  else return ()
  where
    check [] = True
    check ((a', b'):xs)
      | (a', b') == (a, b) || (a', b') == (b, a) = False
      | otherwise = check xs


emptyAllocator = Allocator M.empty 0
emptyLState    = LState emptyAllocator [] []