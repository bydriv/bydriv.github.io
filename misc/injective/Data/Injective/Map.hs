module Data.Injective.Map where

import qualified Control.Monad as Monad
import qualified Data.Maybe    as Maybe

data Map a b =
    Empty
  | Branch a b (Map a b) (Map a b) (Map a b) (Map a b)
  deriving (Eq, Ord, Read, Show)

empty :: Map a b
empty =
  Empty

singleton :: a -> b -> Map a b
singleton x y =
  Branch x y Empty Empty Empty Empty

insert :: (Ord a, Ord b) => a -> b -> Map a b -> Map a b
insert x y Empty =
  Branch x y Empty Empty Empty Empty
insert x y (Branch x' y' t1 t2 t3 t4) =
  case (compare x x', compare y y') of
    (EQ, EQ) ->
      Branch x y t1 t2 t3 t4
    (EQ, _) ->
      error "uniqueness unsatisfied"
    (_, EQ) ->
      error "uniqueness unsatisfied"
    (LT, LT) ->
      Branch x' y' (insert x y t1) t2 t3 t4
    (LT, GT) ->
      Branch x' y' t1 (insert x y t2) t3 t4
    (GT, LT) ->
      Branch x' y' t1 t2 (insert x y t3) t4
    (GT, GT) ->
      Branch x' y' t1 t2 t3 (insert x y t4)

lookupLeft :: (Ord a, Ord b) => a -> Map a b -> Maybe (a, b)
lookupLeft _ Empty =
  Nothing
lookupLeft x (Branch x' y t1 t2 t3 t4) =
  case compare x x' of
    EQ ->
      Just (x', y)
    LT ->
      Monad.mplus (lookupLeft x t1) (lookupLeft x t2)
    GT ->
      Monad.mplus (lookupLeft x t3) (lookupLeft x t4)

lookupRight :: (Ord a, Ord b) => b -> Map a b -> Maybe (a, b)
lookupRight _ Empty =
  Nothing
lookupRight y (Branch x y' t1 t2 t3 t4) =
  case compare y y' of
    EQ ->
      Just (x, y')
    LT ->
      Monad.mplus (lookupRight y t1) (lookupRight y t3)
    GT ->
      Monad.mplus (lookupRight y t2) (lookupRight y t4)

memberLeft :: (Ord a, Ord b) => a -> Map a b -> Bool
memberLeft x f =
  Maybe.isJust (lookupLeft x f)

memberRight :: (Ord a, Ord b) => b -> Map a b -> Bool
memberRight y f =
  Maybe.isJust (lookupRight y f)

fromList :: (Ord a, Ord b) => [(a, b)] -> Map a b
fromList =
  foldr (\(x, y) -> insert x y) empty
