module Rcheck.Main where
import Rcheck.BrMonad
import Rcheck.Solver

test :: Unify a => Br (LState a) Int
test = do
  return 1

main = do
  let a = map fst $ runBr test 2
  putStrLn . show $ a
