# RSolve

[![](https://img.shields.io/hackage/v/RSolve.svg)](https://hackage.haskell.org/package/RSolve)

A general solver for type checkers of programming languages and real world puzzles with complex constraints.

The README is going to get updated.

## Propositional Logic

RSolve uses [Disjunctive normal form](https://en.wikipedia.org/wiki/Disjunctive_normal_form) to solve logic problems.

This disjunctive normal form works naturally with the logic problems where the atom formulas can be generalized to an arbitrary equation in the problem domain by introducing a problem domain specific solver. A vivid
example could be found at `RSolve.HM.Inference`, where
I implemented an extended algo-W for [HM unification](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system).


To take advantage of RSolve, we should implement 2 classes:

- `AtomF`, which stands for the atom formula.

- `CtxSolver`, which stands for the way to solve a bunch of atom formulas.

However we might not need to a solver sometimes:

```haskell
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
    assert $ Not ("a" <==> A)

main =
  let equationGroups = unionEquations equations
  in forM equationGroups print
```
produces
```haskell
[At {at_l = "a", at_r = A},At {at_l = "a", at_r = B}]
[At {at_l = "a", at_r = A},At {at_l = "a", at_r = C}]
[At {at_l = "a", at_r = A},At {at_l = "a", at_r = D}]
[At {at_l = "a", at_r = B}]
[At {at_l = "a", at_r = B},At {at_l = "a", at_r = C}]
[At {at_l = "a", at_r = B},At {at_l = "a", at_r = C},At {at_l = "a", at_r = D}]
[At {at_l = "a", at_r = B},At {at_l = "a", at_r = D}]
```

According to the property of the problem domain, we can figure out that
only the 4-th(1-based indexing) equation group
`[At {at_l = "a", at_r = B}]`
will produce a feasible solution because symbol `a` can
only hold one value.

When do we need a solver? For instance, type checking.

In this case, we need type checking environments to represent the checking states:

```haskell
data TCEnv = TCEnv {
          _noms  :: M.Map Int T  -- nominal type ids
        , _tvars :: M.Map Int T  -- type variables
        , _neqs  :: S.Set (T, T) -- negation constraints
    }
    deriving (Show)

emptyTCEnv = TCEnv M.empty M.empty S.empty
```

For sure we also need to represent the type:

```haskell
data T
    = TVar Int
    | TFresh String
    | T :-> T
    | T :*  T -- tuple
    | TForall (S.Set String) T
    | TApp T T -- type application
    | TNom Int -- nominal type index
    deriving (Eq, Ord)
```

Then the atom formula of HM unification is:

```haskell
data Unif
    = Unif {
          lhs :: T
        , rhs :: T
        , neq :: Bool -- lhs /= rhs or  lhs == rhs?
      }
  deriving (Eq, Ord)
```

We then need to implement this:

```haskell
-- class AtomF a => CtxSolver s a where
--     solve :: a -> MS s ()
prune :: T -> MS TCEnv T -- MS: MultiState
instance CtxSolver TCEnv Unif where
  solver = ...
````

Finally we got this:

```haskell
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
    -- return eqs
    forM_ eqs solve
    return eqs
    a <- prune $ TVar a
    b <- prune $ TVar b
    c <- prune $ TVar c
    return (a, b, c)

test :: Eq a => String -> a -> a -> IO ()
test msg a b
    | a == b = return ()
    | otherwise = print msg

main = do
    forM (unionEquations equations) print

    let (a, b, c):_ = map fst $ runMS solu emptyTCEnv
    test "1 failed" (show a) "@t1 -> @t1 * @t1"
    test "2 failed" (show b) "@t1"
    test "3 failed" (show c) "@t1"
```