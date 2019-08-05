module RSolve.Logic where

-- atom formula
class (Show a, Ord a) => AtomF a where
    -- | Specifies how to handle the negations.
    --   For the finite and enumerable solutions,
    --   we can return its supplmentary set.
    notA :: a -> [a]