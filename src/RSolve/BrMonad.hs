module RSolve.BrMonad where
import Control.Monad

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
