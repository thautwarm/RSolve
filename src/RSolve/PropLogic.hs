-- | Propositional logic infrastructures
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-03
-- License: MIT
{-# LANGUAGE LambdaCase #-}
module RSolve.PropLogic
    (AtomF(..), WFF(..), NF(..), assertNF, normal, assert)
where
import RSolve.Logic
import RSolve.MultiState
import Control.Applicative ((<|>))

infixl 5 :&&, :&&:
infixl 3 :||, :||:

data WFF a
    = Atom a
    | Not  (WFF a)
    | WFF a :&&: WFF a
    | WFF a :||: WFF a

-- normalized WWF
data NF a
    = AtomN a
    | NF a :&& NF a
    | NF a :|| NF a

normal :: AtomF a => WFF a -> NF a
normal = \case
    Atom a -> AtomN a
    p1 :&&: p2  -> normal p1 :&& normal p2
    p1 :||: p2  -> normal p1 :|| normal (Not p1 :&&: p2)
    Not (Atom a) ->
        case map AtomN $ notA a of
            hd:tl -> foldl (\a b -> a :|| b) hd tl
            []    -> error $ "Supplementary set of " ++ show a ++ " is empty!"
    Not (Not p)  -> normal p
    Not (p1 :&&: p2) -> normal (Not p1) :|| normal (Not p2)
    Not (p1 :||: p2) -> normal (Not p1) :&& normal (Not p2)

assertNF :: NF a -> MS [a] ()
assertNF = \case
    AtomN a -> modifyMS (a:)
    p1 :&& p2 -> assertNF p1 >> assertNF p2
    p1 :|| p2 -> assertNF p1 <|> assertNF p2

assert :: AtomF a => WFF a -> MS [a] ()
assert = assertNF . normal