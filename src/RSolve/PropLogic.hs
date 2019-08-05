-- | Propositional logic infrastructures
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-03
-- License: MIT
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module RSolve.PropLogic
    (AtomF(..), WFF(..), NF(..), assertNF, normal, assert, unionEquations)
where

import RSolve.Logic
import RSolve.MultiState
import Control.Applicative ((<|>))
import qualified Data.Set as S

infixl 5 :&&, :&&:
infixl 3 :||, :||:, :=>:

data WFF a
    -- | Atom formula, should be specified by the problem
    = Atom a
    | Not  (WFF a)
    -- | And
    | WFF a :&&: WFF a
    -- | Or
    | WFF a :||: WFF a
    -- | Implication
    | WFF a :=>:  WFF a
    deriving (Functor, Eq, Ord)

-- | normalized WWF, where '[NF a]' the disjunctive normal form.
data NF a
    = AtomN a
    | NF a :&& NF a
    | NF a :|| NF a
    deriving (Functor, Eq, Ord)

normal :: AtomF a => WFF a -> NF a
normal = \case
    Atom a -> AtomN a
    p1 :&&: p2  -> normal p1 :&& normal p2
    p1 :||: p2  -> normal p1 :|| normal (Not p1 :&&: p2)
    Not (Atom a) ->
        case map AtomN $ notA a of
            hd:tl -> foldl (:||) hd tl
            []    -> error $ "Supplementary set of " ++ show a ++ " is empty!"
    Not (Not p)  -> normal p
    Not (p1 :&&: p2) -> normal (Not p1) :|| normal (Not p2)
    Not (p1 :||: p2) -> normal (Not p1) :&& normal (Not p2)
    Not (p1 :=>: p2) -> normal (Not p1 :||: p2)

assertNF :: AtomF a => NF a -> MS (S.Set a) ()
assertNF = \case
    AtomN a   -> modifyMS (S.insert a)
    p1 :&& p2 -> assertNF p1 >> assertNF p2
    p1 :|| p2 -> assertNF p1 <|> assertNF p2


-- | Use a propositinal logic formula to build logic equations
--   incrementally.
assert :: AtomF a => WFF a -> MS (S.Set a) ()
assert = assertNF . normal

-- | Produced a list of disjunctions of conjunctive clauses.
unionEquations :: AtomF a  => MS (S.Set a) () -> [[a]]
unionEquations m =
    -- get states
    let sts = map snd $ runMS m S.empty
    -- unique states
    in map S.toList . S.toList . S.fromList $ sts

