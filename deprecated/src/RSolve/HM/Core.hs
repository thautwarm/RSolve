{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
-- https://github.com/thautwarm/reFining/blob/master/DotNet/reFining/reFining

module RSolve.HM.Core where
import RSolve.BrMonad
import RSolve.Infr
import RSolve.Logic
import Control.Applicative
import qualified Data.Map as M

type Id = Int

data TypeOp = Arrow | Join | Stmt
    deriving (Show, Eq, Ord)

data Prim = Int | Float | Char
    deriving (Show, Eq, Ord)


data Core where
    Prim   :: Prim -> Core

    Op     :: TypeOp -> Core -> Core -> Core

    Forall :: [Id] -> Core -> Core

    Var    :: Id -> Core
    deriving (Eq)

instance Show Core where
    show (Prim a) = show a
    show (Op Arrow a b) =
        "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (Op Join a b) = show a ++ ", " ++ show b
    show (Op Stmt a b) = show a ++ ";\n" ++ show b
    show (Forall xs b)  =
        let f a b = a ++ " a" ++ show b
        in foldl f "forall " xs ++ "." ++ show b
    show (Var a) = "a" ++ show a

free :: M.Map Id Core -> Core -> Core
free m = mkFree
    where
        mkFree a@(Prim _) = a
        mkFree (Op op a b) = Op op (mkFree a) (mkFree b)
        mkFree (Forall a b) = Forall a (mkFree b)
        mkFree a@(Var id) =
            M.findWithDefault a id m

occurIn :: Addr -> Addr -> Br (LState Core) Bool
occurIn l = contains . Var
    where
        contains (Prim _) = return False

        contains (Var a) =
            if a == l then return True
            else tryLoad a >>= \case
                Just a -> contains a
                _ -> return False

        contains (Op _ a b) = (||) <$> contains a <*> contains b
        contains (Forall _ a) = contains a

instance Reference Core where
    mkRef = Var
    isRef (Var a) = Just a
    isRef  _      = Nothing


instance Unify Core where
    prune v@(Var a) = tryLoad a >>= \case
        Just var -> prune var
        _        -> return v

    prune a@(Prim _) = return a

    prune (Forall a b) = Forall a <$> prune b
    prune (Op op a b) = Op op <$> prune a <*> prune b

    unify (Prim a) (Prim b) =
            if a == b then return ()
            else empty

    unify l@(Var a) r@(Var b)
        | a == b    = return ()
        | otherwise = do
            recursive <- occurIn a b
            if recursive
            then error "ill formed definition like a = a -> b"
            else update a r

    unify l r@(Var _) = unify r l

    unify (Var id)  r = update id r

    -- type operators are not frist class
    unify (Op opl l1 l2) (Op opr r1 r2) =
        if opl /= opr then empty
        else
            unify l1 r1 >> unify l2 r2

    unify (Forall freevars poly) r = do
        pairs <- mapM freepair freevars
        let freemap = M.fromList pairs
        let l = free freemap poly
        unify l r
        where
            freepair freevar = (freevar,) <$> mkRef <$> new

    unify l r@(Forall _ _) = unify r l


