-- import RSolve.Options.Example
-- import RSolve.HM.Example


-- test1 =
--     putStrLn "HM unification"   >>
--     hmUnificationExample        >>
--     putStrLn "4-option puzzles" >>
--     optionExample

-- main = print 233


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

import RSolve.PropLogic
import RSolve.MultiState
import Data.List (delete)
import Control.Monad


data Value = A | B | C | D
    deriving (Show, Eq, Ord, Enum)

data At = At {lhs :: String, rhs :: Value}
    deriving (Show, Eq, Ord)

instance AtomF At where
    notA At {lhs = lhs, rhs = rhs} =
        let wholeSet  = enumFrom (toEnum 0) :: [Value]
            contrasts = delete rhs wholeSet
        in [At {lhs = lhs, rhs = rhs'} | rhs' <- contrasts]

infix 6 <=>
(<=>) :: String -> Value -> WFF At
s <=> v = Atom $ At s v
equations = do
    assert $ "a" <=> A :||: "a" <=> B
    assert $ Not ("a" <=> A)
main = forM_ (map snd $ runMS equations []) print
