{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW4.hs EqSet.hs EqMap.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

import Data.Char (chr, ord)
import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqMap (EqMap)
import EqMap qualified
import EqSet (EqSet)
import EqSet qualified

-- Section 2: Serialization
class Serializable a where
  serialize :: a -> [Int]
  deserialize :: [Int] -> a

instance Serializable Int
instance Serializable Bool
instance Serializable Char
instance Serializable a => Serializable (Maybe a)
instance (Serializable a, Serializable b) => Serializable (a, b)
instance (Serializable a, Serializable b) => Serializable (Either a b)
instance Serializable a => Serializable [a]
instance (Serializable a, Eq a) => Serializable (EqSet a)
instance (Serializable k, Eq k, Serializable v) => Serializable (EqMap k v)

-- Section 3: Metric
infinity :: Double
infinity = 1 / 0

class Eq a => Metric a where
  distance :: a -> a -> Double

instance Metric Double
instance Metric Int
instance Metric Char

-- Euclidean distance
instance (Metric a, Metric b) => Metric (a, b)

data ManhattanTuple a b = ManhattanTuple a b deriving Eq
instance (Metric a, Metric b) => Metric (ManhattanTuple a b)

-- Just and Nothing have distance of infinity.
-- Two Justs measure the distance between the two values.
instance Metric a => Metric (Maybe a)

-- Left and Right have a distance of infinity.
-- Same constructores measure the distance between the two values.
instance (Metric a, Metric b) => Metric (Either a b)

-- Lists of different sizes have distance of infinity.
-- Euclidean distance.
instance Metric a => Metric [a]

newtype ManhattanList a = ManhattanList [a] deriving Eq
instance Metric a => Metric (ManhattanList a)

-- Returns the element with the shortest distance to the input.
-- If there are no numbers whose distance is less than infinity, return Nothing.
closest :: Metric a => a -> [a] -> Maybe a
-- Similar to the above, but uses a function move the element
-- to another metric space.
closestOn :: Metric b => (a -> b) -> a -> [a] -> Maybe a
-- Will not swap elements whose distance is less than d, even if their
-- order implies they should be swapped.
metricBubbleSort :: (Metric a, Ord a) => Double -> [a] -> [a]
-- Similar to the above, but uses a function to extract the value used for sorting.
metricBubbleSortOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]

-- Bonus (10 points).
clusters :: Metric a => [a] -> [[a]]
