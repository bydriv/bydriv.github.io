module Data.Injective.Relation
  ( Relation
  , empty
  , singleton
  , selectLeft
  , selectRight
  , fromList ) where

import qualified Data.Injective.Map as Map

newtype Relation a b =
    Relation (Map.Map (a, Suffix b) (b, Suffix a))
  deriving (Eq, Ord, Read, Show)

data Suffix a =
    First
  | Middle a
  | Last
  deriving (Eq, Ord, Read, Show)

empty :: Relation a b
empty = Relation Map.empty

singleton :: a -> b -> Relation a b
singleton x y = Relation (Map.singleton (x, Middle y) (y, Middle x))

selectLeft :: (Ord a, Ord b) => a -> Relation a b -> [(a, b)]
selectLeft x (Relation f) =
  (map (\((x', _), (y, _)) -> (x', y)) (Map.betweenLeft (x, First) (x, Last) f))

selectRight :: (Ord a, Ord b) => b -> Relation a b -> [(a, b)]
selectRight y (Relation f) =
  (map (\((x, _), (y', _)) -> (x, y')) (Map.betweenRight (y, First) (y, Last) f))

fromList :: (Ord a, Ord b) => [(a, b)] -> Relation a b
fromList = Relation . Map.fromList . map (\(x, y) -> ((x, Middle y), (y, Middle x)))
