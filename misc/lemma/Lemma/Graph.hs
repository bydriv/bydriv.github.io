module Lemma.Graph
  ( Graph
  , empty, singleton, insert, member, between
  , isLinearlyOrdered, lookupLeft, lookupRight, searchLeft, searchRight
  , lookupLeftQuarter, lookupRightQuarter
  , rotateLeft, rotateRight ) where

import qualified Lemma.Metric as Metric

data Graph a b =
    Leaf
  | Branch a b (Graph a b) (Graph a b)
  deriving (Read, Show)

empty :: Graph a b
empty =
  Leaf

singleton :: a -> b -> Graph a b
singleton x y =
  Branch x y Leaf Leaf

insert :: (Metric.Metric a, Metric.Metric b) => a -> b -> Graph a b -> Graph a b
insert x y Leaf =
  singleton x y
insert x y (Branch x' y' a b) =
  case compare (Metric.leftDistance x y) (Metric.leftDistance x' y') of
    LT ->
      Branch x' y' (insert x y a) b
    EQ ->
      Branch x' y' a b
    GT ->
      Branch x' y' a (insert x y b)

member :: (Metric.Metric a, Metric.Metric b) => a -> b -> Graph a b -> Bool
member _ _ Leaf =
  False
member x y (Branch x' y' a b) =
  case compare (Metric.leftDistance x y) (Metric.leftDistance x' y') of
    LT ->
      member x y a
    EQ ->
      True
    GT ->
      member x y b

between ::
  (Metric.Metric a, Metric.Metric b) =>
    (a, b) -> (a, b) -> Graph a b -> [(a, b)]
between _ _ Leaf =
  []
between (x1, y1) (x2, y2) (Branch x y a b) =
  case
    ( compare (Metric.leftDistance x1 y1) (Metric.leftDistance x y)
    , compare (Metric.leftDistance x2 y2) (Metric.leftDistance x y) )
  of
    (GT, LT) ->
      []
    (_, LT) ->
      between (x1, y1) (x2, y2) a
    (GT, _) ->
      between (x1, y1) (x2, y2) b
    (_, _) ->
      between (x1, y1) (x2, y2) a ++ (x, y) : between (x1, y1) (x2, y2) b

--------------------------------------------------------------------------------
--  Operations for Linearly Ordered Sets
--------------------------------------------------------------------------------

isLinearlyOrdered :: (Metric.Metric a, Metric.Metric b) => Graph a b -> Bool
isLinearlyOrdered Leaf =
  True
isLinearlyOrdered (Branch x y a b) =
  case (a, b) of
    (Leaf, Leaf) ->
      True
    (Leaf, Branch x1 y1 _ _) ->
      signum (Metric.metric x x1) < 0 && signum (Metric.metric y y1) < 0
        && isLinearlyOrdered b
    (Branch x1 y1 _ _, Leaf) ->
      signum (Metric.metric x x1) >= 0 && signum (Metric.metric y y1) >= 0
        && isLinearlyOrdered a
    (Branch x1 y1 _ _, Branch x2 y2 _ _) ->
      signum (Metric.metric x x1) >= 0 && signum (Metric.metric y y1) >= 0
        && signum (Metric.metric x x2) < 0 && signum (Metric.metric y y2) < 0
        && isLinearlyOrdered a && isLinearlyOrdered b

--  O(log(n)) | isLinearlyOrdered(G)
lookupLeft :: Metric.Metric a => a -> Graph a b -> Maybe b
lookupLeft _ Leaf =
  Nothing
lookupLeft x (Branch x' y' a b) =
  case compare (Metric.metric x x') 0 of
    LT ->
      lookupLeft x a
    EQ ->
      Just y'
    GT ->
      lookupLeft x b

--  O(log(n)) | isLinearlyOrdered(G)
lookupRight :: Metric.Metric b => b -> Graph a b -> Maybe a
lookupRight _ Leaf =
  Nothing
lookupRight y (Branch x' y' a b) =
  case compare (Metric.metric y y') 0 of
    LT ->
      lookupRight y a
    EQ ->
      Just x'
    GT ->
      lookupRight y b

searchLeft :: Metric.Metric a => a -> Graph a b -> [b]
searchLeft _ Leaf =
  []
searchLeft x (Branch x' y' a b) =
  case compare (Metric.metric x x') 0 of
    LT ->
      searchLeft x a
    EQ ->
      y' : searchLeft x a
    GT ->
      searchLeft x b

searchRight :: Metric.Metric b => b -> Graph a b -> [a]
searchRight _ Leaf =
  []
searchRight y (Branch x' y' a b) =
  case compare (Metric.metric y y') 0 of
    LT ->
      searchRight y a
    EQ ->
      x' : searchRight y a
    GT ->
      searchRight y b

--------------------------------------------------------------------------------
--  3/4 logarithmic searching
--------------------------------------------------------------------------------

-- O(log(n)) | 75% elements
-- O(n)      | 25% outliers
lookupLeftQuarter :: Metric.Metric a => a -> Graph a b -> Maybe b
lookupLeftQuarter _ Leaf =
  Nothing
lookupLeftQuarter x (Branch x' y' a b) =
  case compare (Metric.metric x x') 0 of
    LT ->
      case lookupLeftQuarter x a of
        Nothing ->
          lookupLeftQuarter x b
        Just y ->
          Just y
    EQ ->
      Just y'
    GT ->
      case lookupLeftQuarter x b of
        Nothing ->
          lookupLeftQuarter x a
        Just y ->
          Just y

-- O(log(n)) | 75% elements
-- O(n)      | 25% outliers
lookupRightQuarter :: Metric.Metric b => b -> Graph a b -> Maybe a
lookupRightQuarter _ Leaf =
  Nothing
lookupRightQuarter y (Branch x' y' a b) =
  case compare (Metric.metric y y') 0 of
    LT ->
      case lookupRightQuarter y a of
        Nothing ->
          lookupRightQuarter y b
        Just x ->
          Just x
    EQ ->
      Just x'
    GT ->
      case lookupRightQuarter y b of
        Nothing ->
          lookupRightQuarter y a
        Just x ->
          Just x

--------------------------------------------------------------------------------

rotateLeft :: Graph a b -> Graph a b
rotateLeft Leaf =
  Leaf
rotateLeft (Branch x y a Leaf) =
  Branch x y a Leaf
rotateLeft (Branch x y a (Branch x' y' b c)) =
  Branch x' y' (Branch x y a b) c

rotateRight :: Graph a b -> Graph a b
rotateRight Leaf =
  Leaf
rotateRight (Branch x y Leaf b) =
  Branch x y Leaf b
rotateRight (Branch x y (Branch x' y' a b) c) =
  Branch x' y' a (Branch x y b c)
