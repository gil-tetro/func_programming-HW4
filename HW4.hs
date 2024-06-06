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

instance Serializable Int where
  serialize :: Int -> [Int]
  serialize a = [a]
  deserialize :: [Int] -> Int
  deserialize [a] = a
  deserialize _ = error "Invaild input for Int deserialization"

instance Serializable Bool where
  serialize :: Bool -> [Int]
  serialize False = [0]
  serialize True = [1]
  deserialize :: [Int] -> Bool
  deserialize [0] = False
  deserialize [1] = True
  deserialize _ = error "Invaild input for Bool deserialization"

instance Serializable Char where
  serialize :: Char -> [Int]
  serialize c = [ord c]
  deserialize :: [Int] -> Char
  deserialize [c] = chr c
  deserialize _ = error "Invaild input for Char deserializetion"
instance Serializable a => Serializable (Maybe a) where
  serialize :: Maybe a -> [Int]
  serialize Nothing = [0]
  serialize (Just x) = 1 : serialize x

  deserialize :: [Int] -> Maybe a
  deserialize (0 : _) = Nothing
  deserialize (1:xs) = Just (deserialize xs)
  deserialize _ = error "Invalid input for Maybe deserialization"
instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize :: (a, b) -> [Int]
  serialize (a, b) = serialize a ++ serialize b

  deserialize :: [Int] -> (a, b)
  deserialize xs =
    let (aSerialized, bSerialized) = splitAt (length serializedA) xs
        serializedA = serialize (undefined :: a)
    in (deserialize aSerialized, deserialize bSerialized)
instance (Serializable a, Serializable b) => Serializable (Either a b) where
  serialize :: Either a b -> [Int]
  serialize (Left x) = 0 : serialize x
  serialize (Right y) = 1 : serialize y

  deserialize :: [Int] -> Either a b
  deserialize (0:xs) = Left (deserialize xs)
  deserialize (1:xs) = Right (deserialize xs)
  deserialize _ = error "Invalid input for Either deserialization"
instance Serializable a => Serializable [a] where
  serialize xs = length xs : concatMap (\x -> length (serialize x) : serialize x) xs
  deserialize [] = []
  deserialize (x:xs) = deserializeFromList x xs
    where
      deserializeFromList 0 _ = []
      deserializeFromList x' (len:rest) =
          let (n, rest') = splitAt len rest
          in deserialize n : deserializeFromList (x' - 1) rest'
      deserializeFromList _ _ = error "Invalid serialization for list"
instance (Serializable a, Eq a) => Serializable (EqSet a)  where
  serialize = serialize . EqSet.elems
  deserialize = EqSet.fromList . deserialize
instance (Serializable k, Eq k, Serializable v) => Serializable (EqMap k v) where
  serialize m = concatMap serializeLength (EqMap.assocs m) 
    where
      serializeLength (k, v) = length (serialize k) : length (serialize v) : (serialize k ++ serialize v)

  deserialize [] = EqMap.empty
  deserialize (xs : (ys : zs)) = EqMap.insert (deserialize (take xs zs)) (deserialize (take ys (drop xs zs))) (deserialize (drop (xs + ys) zs))
  deserialize _ = error "Invalid serialization for EqMap"

-- Section 3: Metric
infinity :: Double
infinity = 1 / 0

class Eq a => Metric a where
  distance :: a -> a -> Double

instance Metric Double where
  distance :: Double -> Double -> Double
  distance x y = abs (x - y)
instance Metric Int where
  distance :: Int -> Int -> Double
  distance x y = fromIntegral (abs (x - y))
instance Metric Char where
  distance :: Char -> Char -> Double
  distance x y = fromIntegral (abs (ord x - ord y))

-- Euclidean distance
instance (Metric a, Metric b) => Metric (a, b) where
  distance :: (Metric a, Metric b) => (a, b) -> (a, b) -> Double
  distance (x1, y1) (x2, y2) = sqrt $ distance x1 x2 ** 2 + distance y1 y2 ** 2

data ManhattanTuple a b = ManhattanTuple a b deriving Eq
instance (Metric a, Metric b) => Metric (ManhattanTuple a b) where
  distance :: (Metric a, Metric b) => ManhattanTuple a b -> ManhattanTuple a b -> Double
  distance (ManhattanTuple x1 y1) (ManhattanTuple x2 y2) = distance x1 x2 + distance y1 y2


-- Just and Nothing have distance of infinity.
-- Two Justs measure the distance between the two values.
instance Metric a => Metric (Maybe a) where
  distance :: Metric a => Maybe a -> Maybe a -> Double
  distance Nothing Nothing = 0
  distance (Just x1) (Just x2) = distance x1 x2
  distance _ _ = infinity

-- Left and Right have a distance of infinity.
-- Same constructores measure the distance between the two values.
instance (Metric a, Metric b) => Metric (Either a b) where
  distance :: (Metric a, Metric b) => Either a b -> Either a b -> Double
  distance (Left _) (Right _) = infinity
  distance (Right _) (Left _) = infinity
  distance (Left x) (Left y) = distance x y
  distance (Right x) (Right y) = distance x y

-- Lists of different sizes have distance of infinity.
-- Euclidean distance.
instance Metric a => Metric [a] where
  distance :: Metric a => [a] -> [a] -> Double
  distance [] [] = 0
  distance xs ys
    | length xs /= length ys = infinity
    | otherwise = sqrt . sum $ zipWith (\x y -> distance x y ** 2) xs ys

newtype ManhattanList a = ManhattanList [a] deriving Eq
instance Metric a => Metric (ManhattanList a) where
  distance :: Metric a => ManhattanList a -> ManhattanList a -> Double
  distance (ManhattanList xs) (ManhattanList ys)
    | length xs /= length ys = infinity
    | otherwise = sum $ zipWith distance xs ys

-- Returns the element with the shortest distance to the input.
-- If there are no numbers whose distance is less than infinity, return Nothing.
closest :: Metric a => a -> [a] -> Maybe a
closest _ [] = Nothing
closest x xs = Just $ foldr1 (\a b -> if distance x a < distance x b then a else b) xs
-- Similar to the above, but uses a function move the element
-- to another metric space.
closestOn :: Metric b => (a -> b) -> a -> [a] -> Maybe a
closestOn _ _ [] = Nothing
closestOn f y xs = Just $ foldr1 (\a b -> if distance (f a) (f y) < distance (f b) (f y) then a else b) xs
-- Will not swap elements whose distance is less than d, even if their
-- order implies they should be swapped.
metricBubbleSort :: (Metric a, Ord a) => Double -> [a] -> [a]
metricBubbleSort _ [] = []
metricBubbleSort d xs = bubbleSort d xs
  where
    bubbleSort _ [] = []
    bubbleSort _ [x] = [x]
    bubbleSort a (x:y:xz)
      | distance x y >= a && x > y = y : bubbleSort a (x:xz)
      | otherwise = x : bubbleSort a (y:xz)
-- Similar to the above, but uses a function to extract the value used for sorting.
metricBubbleSortOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]
metricBubbleSortOn _ _ [] = []
metricBubbleSortOn f d xs = bubbleSort d xs
  where
    bubbleSort _ [] = []
    bubbleSort _ [x] = [x]
    bubbleSort c (x:y:xz)
      | distance (f x) (f y) >= c && f x > f y = y : bubbleSort c (x:xz)
      | otherwise = x : bubbleSort c (y:xz)

-- Bonus (10 points).
clusters :: Metric a => [a] -> [[a]]
clusters = undefined
