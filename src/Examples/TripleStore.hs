{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module Examples.TripleStore
    ( TripleStore
    , singleton
    , delete
    , insert
    , member
    , triples
    , query
    ) where

import           Optics.Core
import           Optics.TH
import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map.Monoidal (MMap, mat, mitraverse)
import qualified Data.Map.Monoidal as MMap

type TS a b c = MMap a (MMap b (Set c))

singleton' :: (Ord a, Ord b, Ord c) => a -> b -> c -> TS a b c
singleton' a b c = MMap.singleton a $ MMap.singleton b $ Set.singleton c

delete' :: (Ord a, Ord b, Ord c) => a -> b -> c -> TS a b c -> TS a b c
delete' a b c = over (mat a % mat b) $ Set.delete c

member' :: (Ord a, Ord b, Ord c) => a -> b -> c -> TS a b c -> Bool
member' a b c = Set.member c . view (mat a % mat b)

triples' :: forall a b c. (Ord a, Ord b, Ord c) => TS a b c -> [(a, b, c)]
triples' = ifoldMapOf (mitraverse <%> mitraverse % folded) $ \(a, b) c -> [(a, b, c)]

data TripleStore a b c = TripleStore
    { _abc :: TS a b c
    , _bca :: TS b c a
    , _cab :: TS c a b
    }
    deriving (Show, Read, Eq, Ord)

instance (Ord a, Ord b, Ord c) => Semigroup (TripleStore a b c) where
    TripleStore x y z <> TripleStore u v w = TripleStore (x <> u) (y <> v) (z <> w)

instance (Ord a, Ord b, Ord c) => Monoid (TripleStore a b c) where
    mempty = TripleStore mempty mempty mempty

makeLenses ''TripleStore

singleton :: (Ord a, Ord b, Ord c) => a -> b -> c -> TripleStore a b c
singleton a b c = TripleStore 
    (singleton' a b c) 
    (singleton' b c a)
    (singleton' c a b)

insert :: (Ord a, Ord b, Ord c) => a -> b -> c -> TripleStore a b c -> TripleStore a b c
insert a b c = (<> singleton a b c)

delete :: (Ord a, Ord b, Ord c) => a -> b -> c -> TripleStore a b c -> TripleStore a b c
delete a b c = over abc (delete' a b c)
             . over bca (delete' b c a)
             . over cab (delete' c a b)

member :: (Ord a, Ord b, Ord c) => a -> b -> c -> TripleStore a b c -> Bool
member a b c = member' a b c . view abc

triples :: (Ord a, Ord b, Ord c) => TripleStore a b c -> [(a, b, c)]
triples = triples' . view abc

query :: (Ord a, Ord b, Ord c) 
      => Maybe a 
      -> Maybe b 
      -> Maybe c 
      -> TripleStore a b c
      -> [(a, b, c)]
query (Just a) (Just b) (Just c) ts
    | member a b c ts = [(a, b, c)]
    | otherwise       = []
query (Just a) (Just b) Nothing ts = 
    map (a,b,) $ Set.toList $ view (abc % mat a % mat b) ts
query (Just a) Nothing (Just c) ts = 
    map (a,,c) $ Set.toList $ view (cab % mat c % mat a) ts
query Nothing (Just b) (Just c) ts = 
    map (,b,c) $ Set.toList $ view (bca % mat b % mat c) ts
query (Just a) Nothing Nothing ts =
    map (\(b, c) -> (a, b, c)) $ itoListOf (abc % mat a % mitraverse % folded) ts
query Nothing (Just b) Nothing ts =
    map (\(c, a) -> (a, b, c)) $ itoListOf (bca % mat b % mitraverse % folded) ts
query Nothing Nothing (Just c) ts =
    map (\(a, b) -> (a, b, c)) $ itoListOf (cab % mat c % mitraverse % folded) ts
query Nothing Nothing Nothing ts = triples ts

test :: TripleStore Int Int Int
test = insert 1 2 3
     $ insert 1 2 4
       mempty
