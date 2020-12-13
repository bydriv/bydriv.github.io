module Data.Injective.List where

import qualified Data.Injective.Map as Map

newtype List a =
    List (Int, Map.Map (Maybe (Int, a)) (Maybe (Int, a)))
  deriving (Eq, Ord, Read, Show)

nil :: List a
nil = List (0, Map.empty)

cons :: Ord a => a -> List a -> List a
cons x (List (i, f)) =
  case Map.lookupLeft Nothing f of
    Nothing ->
      List (i + 1, Map.fromList [(Nothing, Just (i, x)), (Just (i, x), Nothing)])
    Just (x', y) ->
      List (i + 1, Map.insert x' (Just (i, x)) (Map.insert (Just (i, x)) y (Map.deleteLeft x' f)))

snoc :: Ord a => List a -> a -> List a
snoc (List (i, f)) y =
  case Map.lookupRight Nothing f of
    Nothing ->
      List (i + 1, Map.fromList [(Nothing, Just (i, y)), (Just (i, y), Nothing)])
    Just (x, y') ->
      List (i + 1, Map.insert x (Just (i, y)) (Map.insert (Just (i, y)) y' (Map.deleteRight y' f)))

between :: Ord a => Maybe (Int, a) -> Maybe (Int, a) -> List a -> [a]
between x y (List (i, f)) =
  case Map.lookupLeft x f of
    Nothing ->
      []
    Just (_, Nothing) ->
      []
    Just (_, Just y') ->
      if y == Just y' then
        []
      else
        snd y' : between (Just y') y (List (i, f))

toList :: Ord a => List a -> [a]
toList = between Nothing Nothing
