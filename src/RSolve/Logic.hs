module RSolve.Logic where

-- atom formula
class (Show a, Ord a) => AtomF a where
    notA :: a -> [a]