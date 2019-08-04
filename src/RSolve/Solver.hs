{-# LANGUAGE MultiParamTypeClasses #-}
module RSolve.Solver where
import RSolve.Logic
import RSolve.MultiState

class AtomF a => CtxSolver s a where
    solve :: a -> MS s ()