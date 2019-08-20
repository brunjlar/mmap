{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Map.Monoidal
    ( MMap
    , mat
    , mitraverse
    , toList
    , fromList
    , empty
    , insert
    , singleton
    , delete
    , (!)
    ) where

import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Optics.Core

newtype NonTrivial m = NonTrivial m deriving (Show, Read, Eq, Ord)

nonTrivial :: (Eq m, Monoid m) => Iso' m (Maybe (NonTrivial m))
nonTrivial = iso f g
  where
    f m
        | m == mempty = Nothing
        | otherwise   = Just $ NonTrivial m

    g Nothing               = mempty
    g (Just (NonTrivial m)) = m

newtype MMap a m = MMap (Map a (NonTrivial m))
    deriving (Show, Read, Eq, Ord)

mat :: forall a m. (Ord a, Eq m, Monoid m) => a -> Lens' (MMap a m) m
mat a = coercedTo @(Map a (NonTrivial m)) % at' a % re nonTrivial

toIx :: Traversal s t a b -> IxTraversal () s t a b
toIx t = itraversalVL $ \f -> traverseOf t (f ())

mitraverse :: forall a m. IxTraversal' a (MMap a m) m
mitraverse =    toIx (castOptic $ coercedTo @(Map a (NonTrivial m)))
             %> itraversed
             <% toIx (castOptic coerced)

toList :: MMap a m -> [(a, m)]
toList = itoListOf mitraverse

empty :: MMap a m
empty = MMap Map.empty

insert :: (Ord a, Eq m, Monoid m) => a -> m -> MMap a m -> MMap a m
insert = set . mat

singleton :: (Ord a, Eq m, Monoid m) => a -> m -> MMap a m
singleton a m = insert a m empty

fromList :: (Ord a, Eq m, Monoid m) => [(a, m)] -> MMap a m
fromList = foldl' (flip $ uncurry insert) empty

delete :: (Ord a, Eq m, Monoid m) => a -> MMap a m -> MMap a m
delete a = insert a mempty

(!) :: (Ord a, Eq m, Monoid m) => a -> MMap a m -> m
(!) = view . mat

instance (Ord a, Eq m, Monoid m) => Semigroup (MMap a m) where
    x <> y = foldl' f x $ toList y
      where
        f z (a, m) = over (mat a) (<> m) z

instance (Ord a, Eq m, Monoid m) => Monoid (MMap a m) where
    mempty = empty
