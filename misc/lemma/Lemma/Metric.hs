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
--  | metric x y >= 0 AND metric y z >= 0  ==> abs (metric x z) >= 0
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
