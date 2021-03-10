module Relation
  ( R, Relation
  , empty
  , insert, selectLeft, selectRight, betweenLeft, betweenRight
  , insertBy, selectLeftBy, selectRightBy, betweenLeft, betweenRight ) where

import qualified Data.Char  as Char
import qualified Data.Int   as Int
import qualified Data.List  as List
import qualified Data.Ratio as Ratio
import qualified Data.Word  as Word

class Finite a where
  count :: a -> Integer

class Countable a where
  injection :: a -> Rational

type Relation = R Rational

data R i a b =
    Empty
  | Node i [a] [b] (R i a b) (R i a b)
  deriving (Read, Show)

--------------------------------------------------------------------------------

empty :: R i a b
empty =
  Empty

--------------------------------------------------------------------------------

insert :: (Countable a, Countable b) => a -> b -> Relation a b -> Relation a b
insert =
  insertBy injection injection

selectLeft :: (Countable a, Countable b) => a -> Relation a b -> [b]
selectLeft =
  selectLeftBy injection

selectRight :: (Countable a, Countable b) => b -> Relation a b -> [a]
selectRight =
  selectRightBy injection

betweenLeft :: (Countable a, Countable b) => a -> a -> Relation a b -> [b]
betweenLeft =
  betweenLeftBy injection

betweenRight :: (Countable a, Countable b) => b -> b -> Relation a b -> [a]
betweenRight =
  betweenRightBy injection

--------------------------------------------------------------------------------

insertBy :: Ord i => (a -> i) -> (b -> i) -> a -> b -> R i a b -> R i a b
insertBy f g x y r =
  insertAt f g (f x) [] [y] (insertAt f g (g y) [x] [] r)

selectLeftBy :: Ord i => (a -> i) -> a -> R i a b -> [b]
selectLeftBy f x =
  snd . lookupAt (f x)

selectRightBy :: Ord i => (b -> i) -> b -> R i a b -> [a]
selectRightBy g y =
  fst . lookupAt (g y)

betweenLeftBy :: Ord i => (a -> i) -> a -> a -> R i a b -> [b]
betweenLeftBy f x x' =
  concat . snd . between (f x) (f x')

betweenRightBy :: Ord i => (b -> i) -> b -> b -> R i a b -> [a]
betweenRightBy f x x' =
  concat . fst . between (f x) (f x')

--------------------------------------------------------------------------------

compareBy :: Ord i => (a -> i) -> a -> a -> Ordering
compareBy f x x' =
  compare (f x) (f x')

unionBy :: Ord i => (a -> i) -> [a] -> [a] -> [a]
unionBy =
  foldr . List.insertBy . compareBy

insertAt ::
  Ord i => (a -> i) -> (b -> i) -> i -> [a] -> [b] -> R i a b -> R i a b
insertAt f g i xs ys Empty =
  Node i xs ys Empty Empty
insertAt f g i xs ys (Node j xs' ys' lhs rhs) =
  case compare i j of
    LT ->
      Node j xs' ys' (insertAt f g i xs ys lhs) rhs
    EQ ->
      Node j (unionBy f xs' xs)  (unionBy g ys' ys) lhs rhs
    GT ->
      Node j xs' ys' lhs (insertAt f g i xs ys rhs)

lookupAt :: Ord i => i -> R i a b -> ([a], [b])
lookupAt _ Empty =
  ([], [])
lookupAt i (Node j xs ys lhs rhs) =
  case compare i j of
    LT ->
      lookupAt i lhs
    EQ ->
      (xs, ys)
    GT ->
      lookupAt i rhs

between :: Ord i => i -> i -> R i a b -> ([[a]], [[b]])
between _ _ Empty =
  ([], [])
between i j (Node k xs ys lhs rhs)
  | i > k && k >= j =
      ([], [])
  | i <= k && k < j =
      let
        (xss, yss) = between i j lhs
        (xss', yss') = between i j rhs
      in
        (xss ++ xs : xss', yss ++ ys : yss')
  | i <= k && k >= i =
      between i j lhs
  | i > k && k < i =
      between i j rhs

--------------------------------------------------------------------------------

instance Finite Char where
  count _ =
    toInteger (Char.ord maxBound)

instance Finite Int where
  count _ =
    toInteger (maxBound :: Int) - toInteger (minBound :: Int)

instance Finite Int.Int8 where
  count _ =
    toInteger (maxBound :: Int.Int8) - toInteger (minBound :: Int.Int8)

instance Finite Int.Int16 where
  count _ =
    toInteger (maxBound :: Int.Int16) - toInteger (minBound :: Int.Int16)

instance Finite Int.Int32 where
  count _ =
    toInteger (maxBound :: Int.Int32) - toInteger (minBound :: Int.Int32)

instance Finite Int.Int64 where
  count _ =
    toInteger (maxBound :: Int.Int64) - toInteger (minBound :: Int.Int64)

instance Finite Word.Word where
  count _ =
    toInteger (maxBound :: Word.Word)

instance Finite Word.Word8 where
  count _ =
    toInteger (maxBound :: Word.Word8)

instance Finite Word.Word16 where
  count _ =
    toInteger (maxBound :: Word.Word16)

instance Finite Word.Word32 where
  count _ =
    toInteger (maxBound :: Word.Word32)

instance Finite Word.Word64 where
  count _ =
    toInteger (maxBound :: Word.Word64)

--------------------------------------------------------------------------------

instance Countable Char where
  injection = toRational . Char.ord

instance Countable Int where
  injection = toRational

instance Countable Int.Int8 where
  injection = toRational

instance Countable Int.Int16 where
  injection = toRational

instance Countable Int.Int32 where
  injection = toRational

instance Countable Int.Int64 where
  injection = toRational

instance Countable Word.Word where
  injection = toRational

instance Countable Word.Word8 where
  injection = toRational

instance Countable Word.Word16 where
  injection = toRational

instance Countable Word.Word32 where
  injection = toRational

instance Countable Word.Word64 where
  injection = toRational

--------------------------------------------------------------------------------

instance Countable Integer where
  injection = toRational

instance Integral a => Countable (Ratio.Ratio a) where
  injection = toRational

instance (Finite a, Countable a) => Countable [a] where
  injection [] =
    0
  injection (x : xs) =
    injection x + injection xs * toRational (count x) + 1
