module Data.Injective.Set
  ( Set
  , empty
  , singleton
  , insert
  , member
  , fromList
  , toList
  , balance ) where

import qualified Data.Injective.Map as Map

newtype Set a =
    Set (Map.Map a a)
  deriving (Eq, Ord, Read, Show)

empty :: Set a
empty = Set Map.empty

singleton :: a -> Set a
singleton x = Set (Map.singleton x x)

insert :: Ord a => a -> Set a -> Set a
insert x (Set f) = Set (Map.insert x x f)

member :: Ord a => a -> Set a -> Bool
member x (Set f) = Map.memberLeft x f

fromList :: Ord a => [a] -> Set a
fromList = Set . Map.fromList . map (\x -> (x, x))

toList :: Set a -> [a]
toList (Set f) = map fst (Map.toList f)

balance :: Ord a => Set a -> Set a
balance = fromList . toList
