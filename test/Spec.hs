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

import RSolve.HM
import RSolve.PropLogic
import RSolve.MultiState
import RSolve.Solver
import Control.Monad

import qualified Data.Set as S

import Data.List (delete)
import Control.Monad

data Value = A | B | C | D
    deriving (Show, Eq, Ord, Enum)

data At = At {at_l :: String, at_r :: Value}
    deriving (Show, Eq, Ord)

instance AtomF At where
    notA At {at_l = lhs, at_r = rhs} =
        let wholeSet  = enumFrom (toEnum 0) :: [Value]
            contrasts = delete rhs wholeSet
        in [At {at_l = lhs, at_r = rhs'} | rhs' <- contrasts]

infix 6 <==>
s <==> v = Atom $ At s v
equations = do
    assert $ "a" <==> A :||: "a" <==> B
    assert $ "b" <==> C :||: "b" <==> D
    assert $ Not ("a" <==> A)
    assert $ Not ("a" <==> B :=>: "b" <==> C)


infixl 6 <=>
a <=> b = Atom $ Unif {lhs=a, rhs=b, neq=False}
solu = do
    a <- newTVar
    b <- newTVar
    c <- newTVar
    d <- newTVar
    let [eqs] = unionEquations $
                do
                assert $ TVar a <=> TForall (S.fromList ["s"]) ((TFresh "s") :-> (TFresh "s" :* TFresh "s"))
                assert $ TVar a <=> (TVar b :-> (TVar c :* TVar d))
                assert $ TVar d <=> TNom 1
    forM_ eqs solve
    a <- prune $ TVar a
    b <- prune $ TVar b
    c <- prune $ TVar c
    return (a, b, c)

test :: Eq a => String -> a -> a -> IO ()
test msg a b
    | a == b = return ()
    | otherwise = print msg

main = do
    forM (unionEquations equations) $ \xs ->
        case xs of
            [a, b] -> print xs
            _      -> return ()

    let (a, b, c):_ = map fst $ runMS solu emptyTCEnv
    test "1 failed" (show a) "@t1 -> @t1 * @t1"
    test "2 failed" (show b) "@t1"
    test "3 failed" (show c) "@t1"