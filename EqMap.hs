{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module EqMap (
  EqMap,
  CombiningMap (..),
  empty,
  EqMap.insert, -- To avoid name clash with Data.List.insert
  member,
  remove,
  EqMap.lookup, -- To avoid name clash with Prelude.lookup
  assocs
) where

import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqSet (EqSet)
import EqSet qualified

newtype EqMap k v = EqMap (EqSet (Arg k v))

empty :: EqMap k v
empty = EqMap EqSet.empty
member :: Eq k => k -> EqMap k v -> Bool
member k (EqMap m) = any (\(Arg k' _) -> k == k') (EqSet.elems m)
insert :: Eq k => k -> v -> EqMap k v -> EqMap k v
insert k v (EqMap m) = EqMap (EqSet.insert (Arg k v) (removeKey k m))
  where
    removeKey key = EqSet.remove (Arg key (undefined :: v))
remove :: Eq k => k -> EqMap k v -> EqMap k v
remove k (EqMap m) = EqMap (EqSet.remove (Arg k (undefined :: v)) m)
lookup :: Eq k => k -> EqMap k v -> Maybe v
lookup a (EqMap m) = fmap (\(Arg _ v) -> v) . find (\(Arg k _) -> k == a) $ EqSet.elems m
assocs :: EqMap k v -> [(k, v)]
assocs (EqMap m) = map (\(Arg k v) -> (k, v)) (EqSet.elems m)

-- assocsArg :: EqMap k v -> [Arg k v]
-- assocsArg (EqMap m) = EqSet.elems m

buildMap :: Eq k => [(k , v)] -> EqMap k v
buildMap = foldr ins empty
  where ins (k, v) = EqMap.insert k v

instance (Eq k, Eq v) => Eq (EqMap k v) where
  EqMap xs == EqMap ys = xs == ys
instance (Show k, Show v) => Show (EqMap k v) where
  show (EqMap xs) = "{" ++ intercalate ", " (map showPair (EqSet.elems xs)) ++ "}"
    where
      showPair (Arg k v) = Prelude.show k ++ "->" ++ Prelude.show v
instance Eq k => Semigroup (EqMap k v) where
  (EqMap m1) <> (EqMap m2) = EqMap (m1 <> m2)
instance Eq k => Monoid (EqMap k v) where
  mempty = EqMap EqSet.empty
  mappend = (<>)
newtype CombiningMap k v = CombiningMap {getCombiningMap :: EqMap k v}
instance (Eq k, Semigroup v) => Semigroup (CombiningMap k v)where
  CombiningMap m1 <> CombiningMap m2 = CombiningMap (buildMap (combineAssocs (assocs m1) (assocs m2)))
    where
      combineAssocs [] ys = ys
      combineAssocs xs [] = xs
      combineAssocs ((k1, v1):xs) ys =
        case find (\(k2, _) -> k1 == k2) ys of
          Nothing -> (k1, v1) : combineAssocs xs ys
          Just (_, v2) -> (k1, v1 <> v2) : combineAssocs xs (filter (\(k2, _) -> k1 /= k2) ys)
instance (Eq k, Semigroup v) => Monoid (CombiningMap k v) where
  mempty = CombiningMap (EqMap EqSet.empty)
  mappend = (<>)
