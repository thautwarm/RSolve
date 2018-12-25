module RSolve.HM.Example where
import RSolve.HM.Core
import RSolve.BrMonad
import RSolve.Infr
import RSolve.Logic
import Control.Monad

test = do
    let i = Prim Int
    let f = Prim Float
    let arrow = Op Arrow i f

    -- u means undecided
    u1 <- new
    u2 <- new
    u3 <- new
    u4 <- new

    -- u1 -> u2 where u1, u2 is not generic 
    let arrow_var = Op Arrow (Var u1) (Var u2)
    
    -- int -> int
    let arrow_inst1 = Op Arrow i i

    -- float -> float
    let arrow_inst2 = Op Arrow f f


    let arrow_match = Op Arrow (Var u4) (Var u4)
    
    -- a generic function
    let arrow_generic = Forall [u3] $ Op Arrow (Var u3) (Var u3)

    _ <- solve $ Unify arrow arrow_var
    _ <- solve $ Unify arrow_inst1 arrow_match
    _ <- solve $ Unify arrow_generic arrow_inst1
    _ <- solve $ Unify arrow_generic arrow_inst2
    _ <- solveNeg

    mapM require [Var u1, Var u2, arrow_inst1, arrow_inst2, arrow_generic, arrow_match]

format :: [(String, Core)] -> IO ()
format [] = do
    putStrLn "================="
format ((a, b):xs) = do
    _ <- putStrLn $ a ++ " : " ++ show b
    format xs
formayMany fields lst =
    forM_ [zip fields items | items <- lst] format


hmUnificationExample = do
    let fields = ["u1", "u2", "arrow_inst1", "arrow_inst2", "arrow_generic", "arrow_match"] 
    formayMany fields . map fst $ runBr test emptyLState
