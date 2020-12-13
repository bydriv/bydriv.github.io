module Data.Injective.Quotient
  ( Quotient
  , empty
  , insertBy
  , lookupLeft
  , lookupRight
  ) where

import qualified Data.Injective.Map as Map
import qualified Data.Injective.Set as Set

newtype Quotient a b c d =
    Quotient (Map.Map (Class a c) (Class b d))
  deriving (Eq, Ord, Read, Show)

newtype Class a b = Class (a, Set.Set b)
  deriving (Read, Show)

instance Eq a => Eq (Class a b) where
  Class (a, _) == Class (b, _) = a == b

instance Ord a => Ord (Class a b) where
  compare (Class (a, _)) (Class (b, _)) = compare a b

empty :: Quotient a b c d
empty = Quotient Map.empty

insertBy :: (Ord a, Ord b, Ord c, Ord d) => (c -> a) -> (d -> b) -> c -> d -> Quotient a b c d -> Quotient a b c d
insertBy classifyLeft classifyRight x y (Quotient f) =
  let
    a = classifyLeft x
    b = classifyRight y
  in
    case Map.lookupLeft (Class (a, Set.empty)) f of
      Nothing ->
        Quotient (Map.insert (Class (a, Set.singleton x)) (Class (b, Set.singleton y)) f)
      Just (Class (_, xs), Class (_, ys)) ->
        Quotient (Map.insert (Class (a, Set.balance (Set.insert x xs))) (Class (b, Set.balance (Set.insert y ys))) f)

lookupLeft :: (Ord a, Ord b) => a -> Quotient a b c d -> Maybe ((a, Set.Set c), (b, Set.Set d))
lookupLeft a (Quotient f) =
  case Map.lookupLeft (Class (a, Set.empty)) f of
    Nothing ->
      Nothing
    Just (Class (a', xs), Class (b, ys)) ->
      Just ((a', xs), (b, ys))

lookupRight :: (Ord a, Ord b) => b -> Quotient a b c d -> Maybe ((a, Set.Set c), (b, Set.Set d))
lookupRight b (Quotient f) =
  case Map.lookupRight (Class (b, Set.empty)) f of
    Nothing ->
      Nothing
    Just (Class (a, xs), Class (b', ys)) ->
      Just ((a, xs), (b', ys))
