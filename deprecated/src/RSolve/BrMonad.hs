module RSolve.BrMonad where
import Control.Monad
import Control.Monad.Fail
import Control.Applicative

newtype Br s a = Br {runBr :: s -> [(a, s)]}

instance Functor (Br s) where
  fmap = liftM

instance Applicative (Br s) where
  pure = return
  (<*>)  = ap

instance MonadFail (Br s) where
  fail _ = empty

instance Monad (Br s) where
  m >>= k =
    Br $ \s ->
       let xs = runBr m s
       in join [ runBr (k a) s | (a, s) <- xs]
  return a = Br $ \s -> [(a, s)]

instance Alternative (Br s) where
  empty = Br $ const []
  ma <|> mb = Br $ \s -> runBr ma s ++ runBr mb s

getBy f = Br $ \s -> [(f s, s)]
putBy f = Br $ \s -> [((), f s)]
