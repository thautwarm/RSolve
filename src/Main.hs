module Main where
import RSolve.Options.Example
import RSolve.HM.Example


--- test unification
main = hmUnificationExample


-- test 4-option puzzles
-- main = optionExample


-- test2 = do
--   a <- store $ sol [A, B, C]
--   b <- store $ sol [B, C, D]
--   c <- store $ sol [C]
--   _ <- solve $ a `eq`  b
--   _ <- solve $ b `neq` c
--   _ <- solveNeg  -- `Not` condition requires this
--   _ <- solvePred -- unnecessary
--   mapM require [a, b, c] 
  
-- main = do
--     format ["a", "b", "c"] . nub . L.map fst
--     $ runBr test2 emptyLState
  