{-# LANGUAGE RankNTypes #-}
module RSolve.MapLike where
import Data.Maybe
import Prelude (Bool)
class MapLike m where
    lookup :: forall k v. k -> m k v -> Maybe v
    find   :: forall k v. k -> m k v -> v
    find k m =  fromJust (lookup k m)
    insert :: forall k v. k -> v -> m k v -> m k v
    adjust :: forall k v. (v -> v) -> k -> m k v -> m k v
    member :: forall k v. k -> m k v -> Bool
    update :: forall k v. (v -> Maybe v) -> k -> m k v -> m k v