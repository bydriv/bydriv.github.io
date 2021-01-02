---
url: https://bydriv.github.io/misc/lemma/
title: bydriv.github.io
description: Lemma
thumbnail: https://bydriv.github.io/etc/site/thumbnail.png
---

# Lemma

補題ライブラリ。

## Metric.v

```
Require Import ZArith.
Open Scope Z.

Axiom A : Set.
Axiom f : A -> A -> Z.

Axiom A1 : forall x y : A, x = y <-> f x y = 0.
Axiom A2 : forall x y : A, f x y = -(f y x).
Axiom A3 : forall x y z : A, Z.abs (f x y) + Z.abs (f y z) >= Z.abs (f x z).
Axiom A4 : forall x y z : A, f x y >= 0 /\ f y z >= 0 -> f x z >= 0.

Lemma L1 : forall i j : Z, i >= 0 -> j >= 0 -> i + j >= 0.
Proof.
  intros i j P Q.
  omega.
Qed.

Lemma L2 : forall i : Z, i >= 0 -> i = Z.abs i.
Proof.
  intros i P.
  assert (i >= 0 -> i = i). congruence.
  assert (i <= 0 -> i = -i). omega.
  apply (Zabs_ind (fun j => i = j) i H H0).
Qed.

Lemma L3 : forall x y : A, f x y <= 0 <-> f y x >= 0.
Proof.
  intros x y. rewrite A2. omega.
Qed.

Lemma L4 : forall i j : Z, i - j = i + (- j).
Proof.
  intros i j.
  omega.
Qed.

Lemma L5 : forall i j : Z, i + (- - j) = i + j.
Proof.
  intros i j.
  omega.
Qed.

Theorem T1 : forall x y z : A, f x y >= 0 -> f y z >= 0 -> f x y + f y z >= f x z /\ f x z >= 0.
Proof.
  intros x y z P Q.
  assert (f x z >= 0). apply (A4 x y z (conj P Q)).
  assert (f x y = Z.abs (f x y)). apply (L2 (f x y) P).
  assert (f y z = Z.abs (f y z)). apply (L2 (f y z) Q).
  assert (f x z = Z.abs (f x z)). apply (L2 (f x z) H).
  rewrite H0.
  rewrite H1.
  rewrite H2.
  split.
  apply (A3 x y z).
  omega.
Qed.

Theorem T2 : forall x y z : A, f x y <= 0 -> f y z <= 0 -> f x y + f y z <= f x z /\ f x z <= 0.
Proof.
  intros x y z P Q.
  assert (f z y + f y x >= f z x /\ f z x >= 0).
  apply (T1 z y x (proj1 (L3 y z) Q) (proj1 (L3 x y) P)).
  rewrite (A2 x y).
  rewrite (A2 y z).
  rewrite (A2 x z).
  omega.
Qed.

Theorem T3 : forall x y z : A, f x y >= 0 -> f z y <= 0 -> f x z >= 0.
Proof.
  intros x y z P Q.
  apply (A4 x y z (conj P (proj1 (L3 z y) Q))).
Qed.

Theorem T4 : forall x y z : A, f x y <= 0 -> f z y >= 0 -> f z x >= 0.
Proof.
  intros x y z P Q.
  apply (A4 z y x (conj Q (proj1 (L3 x y) P))).
Qed.

Theorem T5 : forall x y z : A, f x y <= 0 -> f y z <= 0 -> f x z <= 0.
Proof.
  intros x y z P Q.
  apply (L3 x z).
  apply (A4 z y x (conj (proj1 (L3 y z) Q) (proj1 (L3 x y) P))).
Qed.

Theorem T6 : forall x y z : A, f x y >= 0 -> f z y <= 0 -> f z x <= 0.
Proof.
  intros x y z P Q.
  apply (T5 z y x Q (proj2 (L3 y x) P)).
Qed.

Theorem T7 : forall x y z : A, f x y <= 0 -> f z y >= 0 -> f x z <= 0.
Proof.
  intros x y z P Q.
  apply (T5 x y z P (proj2 (L3 y z) Q)).
Qed.
```

## Lemma.Metric

```
module Lemma.Metric
  ( Metric(metric), distance
  , MetricOrd, LeftSign, RightSign, LeftDistance, RightDistance
  , metricOrd, leftSign, rightSign, leftDistance, rightDistance ) where

import qualified Data.Int   as Int
import qualified Data.Ratio as Ratio
import qualified Data.Word  as Word

--------------------------------------------------------------------------------
--  SYNOPSIS
--------------------------------------------------------------------------------
--  | The Metric class
--  ----------------------------------------------------------------------------
--  | x == y                              <==> metric x y == 0
--  | metric x y                           ==  negate (metric y x)
--  | abs (metric x y) + abs (metric y z)  >=  abs (metric x z)
--  | metric x y >= 0 AND metric y z >= 0  ==> metric x z >= 0
--------------------------------------------------------------------------------

class Metric a where
  metric :: a -> a -> Rational

distance :: Metric a => a -> a -> Rational
distance x x' =
  abs (metric x x')

--------------------------------------------------------------------------------

newtype MetricOrd a =
    MetricOrd a
  deriving (Read, Show)

metricOrd :: Metric a => a -> MetricOrd a
metricOrd =
  MetricOrd

--------------------------------------------------------------------------------

instance Metric a => Eq (MetricOrd a) where
  MetricOrd x == MetricOrd x' =
    metric x x' == 0

instance Metric a => Ord (MetricOrd a) where
  compare (MetricOrd x) (MetricOrd x') =
    compare (metric x x') 0

--------------------------------------------------------------------------------

newtype LeftSign a b =
    LeftSign (a, b)
  deriving (Read, Show)

newtype RightSign a b =
    RightSign (a, b)
  deriving (Read, Show)

newtype LeftDistance a b =
    LeftDistance (a, b)
  deriving (Read, Show)

newtype RightDistance a b =
    RightDistance (a, b)
  deriving (Read, Show)

leftSign :: (Metric a, Metric b) => a -> b -> LeftSign a b
leftSign x y =
  LeftSign (x, y)

rightSign :: (Metric a, Metric b) => a -> b -> RightSign a b
rightSign x y =
  RightSign (x, y)

leftDistance :: (Metric a, Metric b) => a -> b -> LeftDistance a b
leftDistance x y =
  LeftDistance (x, y)

rightDistance :: (Metric a, Metric b) => a -> b -> RightDistance a b
rightDistance x y =
  RightDistance (x, y)

--------------------------------------------------------------------------------

instance (Metric a, Metric b) => Metric (LeftSign a b) where
  metric (LeftSign (x, y)) (LeftSign (x', y')) =
      if p /= 0 then
        signum p * (abs p + abs q)
      else
        signum q * (abs p + abs q)
    where
      p = metric x x'
      q = metric y y'

instance (Metric a, Metric b) => Metric (RightSign a b) where
  metric (RightSign (x, y)) (RightSign (x', y')) =
      if q /= 0 then
        signum q * (abs p + abs q)
      else
        signum p * (abs p + abs q)
    where
      p = metric x x'
      q = metric y y'

instance (Metric a, Metric b) => Metric (LeftDistance a b) where
  metric (LeftDistance (x, y)) (LeftDistance (x', y')) =
      if p' /= 0 then
        signum p' * (abs p' + abs q')
      else
        signum q' * (abs p' + abs q')
    where
      p = metric x x'
      q = metric y y'
      p' = p + q
      q' = p - q

instance (Metric a, Metric b) => Metric (RightDistance a b) where
  metric (RightDistance (x, y)) (RightDistance (x', y')) =
      if q' /= 0 then
        signum q' * (abs p' + abs q')
      else
        signum p' * (abs p' + abs q')
    where
      p = metric x x'
      q = metric y y'
      p' = p + q
      q' = p - q

--------------------------------------------------------------------------------

instance (Metric a, Metric b) => Eq (LeftSign a b) where
  xy == xy' =
    MetricOrd xy == MetricOrd xy'

instance (Metric a, Metric b) => Eq (RightSign a b) where
  xy == xy' =
    MetricOrd xy == MetricOrd xy'

instance (Metric a, Metric b) => Eq (LeftDistance a b) where
  xy == xy' =
    MetricOrd xy == MetricOrd xy'

instance (Metric a, Metric b) => Eq (RightDistance a b) where
  xy == xy' =
    MetricOrd xy == MetricOrd xy'

--------------------------------------------------------------------------------

instance (Metric a, Metric b) => Ord (LeftSign a b) where
  compare xy xy' =
    compare (MetricOrd xy) (MetricOrd xy')

instance (Metric a, Metric b) => Ord (RightSign a b) where
  compare xy xy' =
    compare (MetricOrd xy) (MetricOrd xy')

instance (Metric a, Metric b) => Ord (LeftDistance a b) where
  compare xy xy' =
    compare (MetricOrd xy) (MetricOrd xy')

instance (Metric a, Metric b) => Ord (RightDistance a b) where
  compare xy xy' =
    compare (MetricOrd xy) (MetricOrd xy')

--------------------------------------------------------------------------------

instance (Metric a, Metric b) => Metric (a, b) where
  metric (x, y) (x', y') =
    metric (leftSign x y) (leftSign x' y')

instance (Metric a, Metric b) => Metric (Either a b) where
  metric (Left x) (Left x') =
    metric x x'
  metric (Right y) (Right y') =
    metric y y'
  metric (Left _) (Right _) =
    -1
  metric (Right _) (Left _) =
    1

instance (Metric a) => Metric (Maybe a) where
  metric Nothing Nothing =
    0
  metric Nothing (Just _) =
    -1
  metric (Just _) Nothing =
    1
  metric (Just x) (Just x') =
    metric x x'

instance (Metric a) => Metric [a] where
  metric [] [] =
    0
  metric [] _ =
    -1
  metric _ [] =
    1
  metric (x : xs) (x' : xs') =
    metric (leftSign x xs) (leftSign x' xs')

--------------------------------------------------------------------------------

instance Metric Integer where
  metric i j =
    toRational i - toRational j

instance Integral a => Metric (Ratio.Ratio a) where
  metric p q =
    toRational p - toRational q

--------------------------------------------------------------------------------

instance Metric Int.Int8 where
  metric i j =
    toRational i - toRational j

instance Metric Int.Int16 where
  metric i j =
    toRational i - toRational j

instance Metric Int.Int32 where
  metric i j =
    toRational i - toRational j

instance Metric Int.Int64 where
  metric i j =
    toRational i - toRational j

--------------------------------------------------------------------------------

instance Metric Word.Word8 where
  metric n m =
    toRational n - toRational m

instance Metric Word.Word16 where
  metric n m =
    toRational n - toRational m

instance Metric Word.Word32 where
  metric n m =
    toRational n - toRational m

instance Metric Word.Word64 where
  metric n m =
    toRational n - toRational m

--------------------------------------------------------------------------------

instance Metric () where
  metric () () =
    0

instance Metric Bool where
  metric x y =
    case compare x y of
      LT ->
        -1
      EQ ->
        0
      GT ->
        1

instance Metric Ordering where
  metric x y =
    case compare x y of
      LT ->
        negate (toRational (length [x .. y]) - 1)
      EQ ->
        0
      GT ->
        toRational (length [y .. x]) - 1

instance Metric Char where
  metric c c' =
    toRational (fromEnum c) - toRational (fromEnum c')

instance Metric Int where
  metric i j =
    toRational i - toRational j

instance Metric Float where
  metric x y =
    toRational x - toRational y

instance Metric Double where
  metric x y =
    toRational x - toRational y
```

## Lemma.Graph

```
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
```
