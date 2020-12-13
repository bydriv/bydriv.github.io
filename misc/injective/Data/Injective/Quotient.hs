module Data.Injective.Quotient
  ( Quotient
  , empty
  , insertBy
  , lookupLeft
  , lookupRight
  ) where

import qualified Data.Injective.Map as Map

newtype Quotient a b c d =
    Quotient (Map.Map (Class a c) (Class b d))
  deriving (Eq, Ord, Read, Show)

newtype Class a b = Class (a, [b])
  deriving (Read, Show)

instance Eq a => Eq (Class a b) where
  Class (a, _) == Class (b, _) = a == b

instance Ord a => Ord (Class a b) where
  compare (Class (a, _)) (Class (b, _)) = compare a b

empty :: Quotient a b c d
empty = Quotient Map.empty

insertBy :: (Ord a, Ord b) => (c -> a) -> (d -> b) -> c -> d -> Quotient a b c d -> Quotient a b c d
insertBy classifyLeft classifyRight x y (Quotient f) =
  let
    a = classifyLeft x
    b = classifyRight y
  in
    case Map.lookupLeft (Class (a, [])) f of
      Nothing ->
        Quotient (Map.insert (Class (a, [x])) (Class (b, [y])) f)
      Just (Class (_, xs), Class (_, ys)) ->
        Quotient (Map.insert (Class (a, x : xs)) (Class (b, y : ys)) f)

lookupLeft :: (Ord a, Ord b) => a -> Quotient a b c d -> Maybe ((a, [c]), (b, [d]))
lookupLeft a (Quotient f) =
  case Map.lookupLeft (Class (a, [])) f of
    Nothing ->
      Nothing
    Just (Class (a', xs), Class (b, ys)) ->
      Just ((a', xs), (b, ys))

lookupRight :: (Ord a, Ord b) => b -> Quotient a b c d -> Maybe ((a, [c]), (b, [d]))
lookupRight b (Quotient f) =
  case Map.lookupRight (Class (b, [])) f of
    Nothing ->
      Nothing
    Just (Class (a, xs), Class (b', ys)) ->
      Just ((a, xs), (b', ys))
