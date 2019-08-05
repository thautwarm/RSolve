{-# LANGUAGE MultiParamTypeClasses #-}
module RSolve.Solver where
import RSolve.Logic
import RSolve.MultiState

class AtomF a => CtxSolver s a where
    -- | Give a atom formula and solve it
    solve :: a -> MS s ()