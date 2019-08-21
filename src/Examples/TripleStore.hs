{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Examples.TripleStore
    ( TripleStore
    , singleton
    , delete
    , insert
    , member
    , triples
    ) where

import           Optics.Core
import           Optics.TH
import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map.Monoidal (MMap)
import qualified Data.Map.Monoidal as MMap

newtype TS a b c = TS (MMap a (MMap b (Set c)))
    deriving (Show, Read, Eq, Ord, Semigroup, Monoid)

singleton' :: (Ord a, Ord b, Ord c) => a -> b -> c -> TS a b c
singleton' a b c = TS $ MMap.singleton a $ MMap.singleton b $ Set.singleton c

_ab :: (Ord a, Ord b, Ord c) => a -> b -> Lens' (TS a b c) (Set c)
_ab a b = coerced % MMap.mat a % MMap.mat b

delete' :: (Ord a, Ord b, Ord c) => a -> b -> c -> TS a b c -> TS a b c
delete' a b c = over (_ab a b) $ Set.delete c

member' :: (Ord a, Ord b, Ord c) => a -> b -> c -> TS a b c -> Bool
member' a b c = Set.member c . view (_ab a b)

triples' :: forall a b c. (Ord a, Ord b, Ord c) => TS a b c -> [(a, b, c)]
triples' = ifoldMapOf _abc $ \(a, b) c -> [(a, b, c)]
  where
    _abc =    coercedTo @(MMap a (MMap b (Set c))) 
           %  MMap.mitraverse 
          <%> MMap.mitraverse 
           %  folded

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
