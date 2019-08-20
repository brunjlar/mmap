{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Set.NonEmpty
    ( NonEmpty
    , fromSet
    , toSet
    , notEmptied
    , _NonEmpty
    , singleton
    , insert
    , member
    , delete
    , maxView
    , minView
    ) where

import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Optics.Core

newtype NonEmpty a = NonEmpty (Set a)
    deriving (Show, Read, Eq, Ord, Semigroup, Foldable)

fromSet :: Set a -> Maybe (NonEmpty a)
fromSet s = if Set.null s then Nothing else Just $ NonEmpty s

toSet :: NonEmpty a -> Set a
toSet (NonEmpty s) = s

notEmptied :: Iso' (Set a) (Maybe (NonEmpty a))
notEmptied = iso fromSet $ maybe Set.empty toSet

_NonEmpty :: Prism' (Set a) (NonEmpty a)
_NonEmpty = notEmptied % _Just

singleton :: a -> NonEmpty a
singleton = NonEmpty . Set.singleton

insert :: Ord a => a -> NonEmpty a -> NonEmpty a
insert a = NonEmpty . Set.insert a . toSet

member :: Ord a => a -> NonEmpty a -> Bool
member a = Set.member a . toSet

delete :: Ord a => a -> NonEmpty a -> Maybe (NonEmpty a)
delete a s = fromSet $ Set.delete a $ toSet s

maxView :: NonEmpty a -> (a, Set a)
maxView = fromJust . Set.maxView . toSet

minView :: NonEmpty a -> (a, Set a)
minView = fromJust . Set.minView . toSet
