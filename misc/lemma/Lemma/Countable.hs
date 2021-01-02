module Lemma.Countable
  ( Countable(injection, surjection) ) where

import qualified Lemma.Metric as Metric

-- injection x >= 0
-- injection . surjection == id <=> at most countable
-- surjection . injection == id <=> countably infinite

class Countable a where
  injection :: a -> Integer
  surjection :: Integer -> a

-- Finite
instance Countable () where
  injection () =
    0

  surjection _ =
    ()

-- Countably Infinite
instance Countable Integer where
  injection i =
    if i < 0 then
      negate i * 2 - 1
    else
      i * 2

  surjection n =
    if even n then
      n `div` 2
    else
      negate ((n + 1) `div` 2)

newtype WellOrdering a =
    WellOrdering a
  deriving (Read, Show)

instance Countable a => Eq (WellOrdering a) where
  WellOrdering x == WellOrdering x' =
    injection x == injection x'

instance Countable a => Ord (WellOrdering a) where
  compare (WellOrdering x) (WellOrdering x') =
    compare (injection x) (injection x')

instance Countable a => Metric.Metric (WellOrdering a) where
   metric (WellOrdering x) (WellOrdering x') =
     toRational (injection x - injection x')
