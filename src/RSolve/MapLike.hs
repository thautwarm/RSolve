{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module RSolve.MapLike where
import Data.Maybe
import Data.Kind
import Prelude hiding (lookup, insert)
import qualified Data.Map  as M
import qualified Data.List as L
import qualified Data.Set  as S

class MapLike m k v | m -> k, m -> v where
    lookup :: k -> m -> Maybe v
    (!)    :: m -> k -> v
    m ! k  = fromJust (lookup k m)
    insert :: k -> v -> m -> m
    adjust :: (v -> v) -> k -> m -> m
    member :: k -> m -> Bool
    update :: (v -> Maybe v) -> k -> m -> m


instance Ord k => MapLike (M.Map k v) k v where
    lookup = M.lookup
    (!) = (M.!)
    insert = M.insert
    adjust = M.adjust
    member = M.member
    update = M.update

instance Eq k => MapLike [(k, v)] k v where
    lookup = L.lookup
    insert k v = \case
        []   -> [(k, v)]
        (k', v'):xs | k' == k -> (k, v):xs
        x:xs -> x:insert k v xs
    adjust f k = \case
        []   -> []
        (k', v):xs | k' == k -> (k', f v):xs
        x:xs -> x:adjust f k xs
    k `member` m = case lookup k m of
            Just  _ -> True
            Nothing -> False
    update f k = \case
        [] -> []
        hd@(k', v):xs | k == k' ->
            case f v of
                Just v'  -> (k, v'):tl
                Nothing  -> hd:tl
            where tl = update f k xs