module Data.Injective.Map
  ( Map
  , empty
  , singleton
  , insert
  , insertAll
  , lookupLeft
  , lookupRight
  , deleteLeft
  , deleteRight
  , minimumLeft
  , minimumRight
  , maximumLeft
  , maximumRight
  , memberLeft
  , memberRight
  , betweenLeft
  , betweenRight
  , union
  , foldLeft
  , foldRight
  , fromList
  , toList
  ) where

import qualified Control.Monad as Monad
import qualified Data.List     as List
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

union :: (Ord a, Ord b) => Map a b -> Map a b -> Map a b
union = insertAll . toList

deleteLeft :: (Ord a, Ord b) => a -> Map a b -> Map a b
deleteLeft _ Empty =
  Empty
deleteLeft x (Branch x' y t1 t2 t3 t4) =
  case compare x x' of
    EQ ->
      union t1 (union t2 (union t3 t4))
    LT ->
      Branch x' y (deleteLeft x t1) (deleteLeft x t2) t3 t4
    GT ->
      Branch x' y t1 t2 (deleteLeft x t3) (deleteLeft x t4)

deleteRight :: (Ord a, Ord b) => b -> Map a b -> Map a b
deleteRight _ Empty =
  Empty
deleteRight y (Branch x y' t1 t2 t3 t4) =
  case compare y y' of
    EQ ->
      union t1 (union t2 (union t3 t4))
    LT ->
      Branch x y' (deleteRight y t1) t2 (deleteRight y t3) t4
    GT ->
      Branch x y' t1 (deleteRight y t2) t3 (deleteRight y t4)

minimumLeft :: (Ord a, Ord b) => Map a b -> Maybe a
minimumLeft Empty =
  Nothing
minimumLeft (Branch x _ t1 t2 _ _) =
  Monad.msum [minimumLeft t1, minimumLeft t2, Just x]

maximumLeft :: (Ord a, Ord b) => Map a b -> Maybe a
maximumLeft Empty =
  Nothing
maximumLeft (Branch x _ _ _ t3 t4) =
  Monad.msum [maximumLeft t3, maximumLeft t4, Just x]

minimumRight :: (Ord a, Ord b) => Map a b -> Maybe b
minimumRight Empty =
  Nothing
minimumRight (Branch _ y t1 _ t3 _) =
  Monad.msum [minimumRight t1, minimumRight t3, Just y]

maximumRight :: (Ord a, Ord b) => Map a b -> Maybe b
maximumRight Empty =
  Nothing
maximumRight (Branch _ y _ t2 _ t4) =
  Monad.msum [maximumRight t2, maximumRight t4, Just y]

insertAll :: (Ord a, Ord b) => [(a, b)] -> Map a b -> Map a b
insertAll xys index =
  case index of
    Empty ->
      fromList' (List.sort xys)
    Branch x y t1 t2 t3 t4 ->
      let
        (xys1, xys2, xys3, xys4) =
          List.foldl' (classify (x, y)) ([], [], [], []) xys
      in
        Branch x y (insertAll xys1 t1) (insertAll xys2 t2) (insertAll xys3 t3) (insertAll xys4 t4)
  where
    fromList' xys' =
      case splitAt (length xys' `div` 2) xys' of
        ([], []) ->
          Empty
        (xys1, []) ->
          fromList' xys1
        (xys1, (x, y) : xys2) ->
          let
            xys3 = List.foldl' (classify (x, y)) ([], [], [], []) xys1
            (xys4, xys5, xys6, xys7) = List.foldl' (classify (x, y)) xys3 xys2
          in
            Branch x y (fromList' xys4) (fromList' xys5) (fromList' xys6) (fromList' xys7)

    classify (x, y) (t1, t2, t3, t4) (x', y') =
      case (compare x' x, compare y' y) of
        (EQ, EQ) ->
          (t1, t2, t3, t4)
        (EQ, _) ->
          error "uniqueness unsatisfied"
        (_, EQ) ->
          error "uniqueness unsatisfied"
        (LT, LT) ->
          ((x', y') : t1, t2, t3, t4)
        (LT, GT) ->
          (t1, (x', y') : t2, t3, t4)
        (GT, LT) ->
          (t1, t2, (x', y') : t3, t4)
        (GT, GT) ->
          (t1, t2, t3, (x', y') : t4)

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

betweenLeft :: (Ord a, Ord b) => a -> a -> Map a b -> [(a, b)]
betweenLeft _ _ Empty =
  []
betweenLeft x1 x2 (Branch x' y t1 t2 t3 t4) =
  case (compare x1 x', compare x2 x') of
    (GT, _) ->
      []
    (_, LT) ->
      []
    (EQ, EQ) ->
      [(x', y)]
    (EQ, GT) ->
      (x', y) : Monad.mplus (betweenLeft x1 x2 t1) (betweenLeft x1 x2 t2)
    (LT, EQ) ->
      (x', y) : Monad.mplus (betweenLeft x1 x2 t3) (betweenLeft x1 x2 t4)
    (LT, GT) ->
      (x', y) : Monad.msum [betweenLeft x1 x2 t1, betweenLeft x1 x2 t2, betweenLeft x1 x2 t3, betweenLeft x1 x2 t4]

betweenRight :: (Ord a, Ord b) => b -> b -> Map a b -> [(a, b)]
betweenRight _ _ Empty =
  []
betweenRight y1 y2 (Branch x y' t1 t2 t3 t4) =
  case (compare y1 y', compare y2 y') of
    (GT, _) ->
      []
    (_, LT) ->
      []
    (EQ, EQ) ->
      [(x, y')]
    (EQ, GT) ->
      (x, y') : Monad.mplus (betweenRight y1 y2 t1) (betweenRight y1 y2 t3)
    (LT, EQ) ->
      (x, y') : Monad.mplus (betweenRight y1 y2 t2) (betweenRight y1 y2 t4)
    (LT, GT) ->
      (x, y') : Monad.msum [betweenRight y1 y2 t1, betweenRight y1 y2 t2, betweenRight y1 y2 t3, betweenRight y1 y2 t4]

foldLeft :: (c -> a -> b -> c) -> c -> Map a b -> c
foldLeft _ z Empty =
  z
foldLeft f z (Branch x y t1 t2 t3 t4) =
  foldLeft f (foldLeft f (f (foldLeft f (foldLeft f z t1) t2) x y) t3) t4

foldRight :: (a -> b -> c -> c) -> c -> Map a b -> c
foldRight _ z Empty =
  z
foldRight f z (Branch x y t1 t2 t3 t4) =
  foldRight f (foldRight f (f x y (foldRight f (foldRight f z t4) t3)) t2) t1

fromList :: (Ord a, Ord b) => [(a, b)] -> Map a b
fromList = flip insertAll Empty

toList :: Map a b -> [(a, b)]
toList = foldRight (\x y xys -> (x, y) : xys) []
