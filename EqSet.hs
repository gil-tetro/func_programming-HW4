{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EqSet (
  EqSet,
  empty,
  EqSet.insert, -- To avoid name clash with Data.List.insert
  EqSet.fromList,
  member,
  remove,
  elems,
) where

import Data.Either
import Data.List
import Data.Maybe

newtype EqSet a = EqSet {getSet :: [a]}

empty :: EqSet a
empty = EqSet []
member :: Eq a => a -> EqSet a -> Bool
member x = elem x . getSet
insert :: Eq a => a -> EqSet a -> EqSet a
insert x (EqSet xs)
  | x `elem` xs = EqSet xs
  | otherwise = EqSet (x:xs)
remove :: Eq a => a -> EqSet a -> EqSet a
remove x (EqSet xs) = EqSet (filter (/= x) xs)
elems :: EqSet a -> [a]
elems (EqSet xs) = xs

fromList :: (Eq a) => [a] -> EqSet a
fromList = foldr EqSet.insert empty

instance Eq a => Eq (EqSet a) where
  (EqSet xs) == (EqSet ys) = null (xs \\ ys) && null (ys \\ xs)
instance Show a => Show (EqSet a) where
  show (EqSet xs) = "{" ++ intercalate ", " (map show xs) ++ "}"
instance Eq a => Semigroup (EqSet a) where
  (EqSet xs) <> (EqSet ys) = EqSet (xs `union` ys)
instance Eq a => Monoid (EqSet a) where
  mempty = EqSet []
  mappend = (<>)
