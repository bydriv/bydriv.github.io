{-# LANGUAGE Safe #-}

module Data.Injective.Map where

import qualified Data.Injective.Index as Index

newtype Map a b =
    Map (Index.Index (Either a b))
  deriving (Eq, Ord, Read, Show)

empty :: Map a b
empty = Map Index.empty

singleton :: a -> b -> Map a b
singleton x y = Map (Index.singleton [Left x, Right y])

insert :: (Ord a, Ord b) => a -> b -> Map a b -> Map a b
insert x y (Map index) = Map (Index.insert [Left x, Right y] index)

lookupLeft :: (Ord a, Ord b) => a -> Map a b -> Maybe (a, b)
lookupLeft x (Map index) =
  case Index.lookupAt 0 (Left x) index of
    Just [Left x', Right y] ->
      Just (x', y)
    _ ->
      Nothing

lookupRight :: (Ord a, Ord b) => b -> Map a b -> Maybe (a, b)
lookupRight y (Map index) =
  case Index.lookupAt 1 (Right y) index of
    Just [Left x, Right y'] ->
      Just (x, y')
    _ ->
      Nothing

fromList :: (Ord a, Ord b) => [(a, b)] -> Map a b
fromList = Map . Index.fromList . map (\(x, y) -> [Left x, Right y])

toList :: Map a b -> [(a, b)]
toList (Map index) = concatMap f (Index.toList index) where
  f [Left x, Right y] = [(x, y)]
  f _ = []
