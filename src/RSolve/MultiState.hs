-- | state monads extended to have branches
-- Author: Taine Zhao(thautwarm)
-- Date: 2018-12
-- License: MIT
module RSolve.MultiState where
import Control.Monad
import Control.Monad.Fail
import Control.Applicative

newtype MS s a = MS {runMS :: s -> [(a, s)]}

instance Functor (MS s) where
  fmap = liftM

instance Applicative (MS s) where
  pure = return
  (<*>)  = ap

instance MonadFail (MS s) where
  fail _ = empty

instance Monad (MS s) where
  m >>= k =
    MS $ \s ->
       let xs = runMS m s
       in join [runMS (k a) s' | (a, s') <- xs]
  return a = MS $ \s -> [(a, s)]

instance Alternative (MS s) where
  empty = MS $ const []
  ma <|> mb = MS $ \s -> runMS ma s ++ runMS mb s

getMS  :: MS s s
getMS = MS $ \s -> [(s, s)]

putMS  :: s -> MS s ()
putMS s = MS $ const [((), s)]

getsMS :: (s -> a) -> MS s a
getsMS f = MS $ \s -> [(f s, s)]

modifyMS :: (s -> s) -> MS s ()
modifyMS f = MS $ \s -> [((), f s)]