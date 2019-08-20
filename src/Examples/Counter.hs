module Examples.Counter
    ( count
    ) where

import Control.Arrow (second)
import Data.List (foldl')
import Data.Monoid (Sum (..))
import Numeric.Natural
import Optics.Core

import Data.Map.Monoidal

count :: Ord a => [a] -> [(a, Natural)]
count = map (second getSum) . toList . foldl' f empty
  where
    f x a = over (mat a) (<> Sum 1) x
