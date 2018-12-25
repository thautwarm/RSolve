# RSolve

A general solver for type checkers of programming languages and real world puzzles with complex constraints. 


## Preview

Here are 2 special cases presented in the following sections to show how powerful `RSolve` is.

### The Most Graceful Hindley-Milner Unification

Check `RSolve.HM.Core` and `RSolve.HM.Example`.  

Uncomment the code in `Main.hs` could reproduce following program:

```haskell
check = do
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
    -- a generic function
    let arrow_generic = Forall [u3] $ Op Arrow (Var u3) (Var u3)

    let arrow_match = Op Arrow (Var u4) (Var u4)

    _ <- solve $ Unify arrow arrow_var
    _ <- solve $ Unify arrow_inst1 arrow_match
    _ <- solve $ Unify arrow_generic arrow_inst1
    _ <- solve $ Unify arrow_generic arrow_inst2
    _ <- solveNeg

    mapM require [Var u1, Var u2, arrow_inst1, arrow_inst2, arrow_generic, arrow_match]
  
```

output:

```
u1 : Int
u2 : Float
arrow_inst1 : (Int -> Int)
arrow_inst2 : (Float -> Float)
arrow_generic : forall  a2.(a2 -> a2)
arrow_match : (Int -> Int)
```

### N-Option Puzzles

This implememtation is presented at `RSolve.Options`,  which provides the abstractions to solve all kinds of puzzles described with options.

A Hello World program could be found at `src/Main.hs` which solves a complex problem described with following link:

https://www.zhihu.com/question/68411978/answer/558913247.


However, the much easier cases taking the same background as above problem (logic constraints described with four options `A, B, C, D`) could be enjoyale:

```haskell
test2 = do
  a <- store $ sol [A, B, C]
  b <- store $ sol [B, C, D]
  c <- store $ sol [C]
  _ <- solve $ a `eq`  b
  _ <- solve $ b `neq` c
  _ <- solveNeg  -- `Not` condition requires this
  _ <- solvePred -- unnecessary
  mapM require [a, b, c] 
  
main = do
    format ["a", "b", "c"] . nub . L.map fst
    $ runBr test2 emptyLState
```

output:

```
λ stack exec RSolve
====
"a" : Sol (fromList [B])
"b" : Sol (fromList [B])
"c" : Sol (fromList [C])
```
