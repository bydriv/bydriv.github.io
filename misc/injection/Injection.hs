module Data.Injection where

import qualified Control.Monad as Monad
import qualified Data.List     as List
import qualified Data.Maybe    as Maybe

class Injection f where
  empty :: (Ord a, Ord b) => f a b
  insert :: (Ord a, Ord b) => a -> b -> f a b -> f a b
  lookupLeft :: (Ord a, Ord b) => a -> f a b -> Maybe b
  lookupRight :: (Ord a, Ord b) => b -> f a b -> Maybe a

singleton :: (Injection f, Ord a, Ord b) => a -> b -> f a b
singleton x y = insert x y empty

memberLeft :: (Injection f, Ord a, Ord b) => a -> f a b -> Bool
memberLeft x f = Maybe.isJust (lookupLeft x f)

memberRight :: (Injection f, Ord a, Ord b) => b -> f a b -> Bool
memberRight y f = Maybe.isJust (lookupRight y f)

fromAList :: (Injection f, Ord a, Ord b) => [(a, b)] -> f a b
fromAList = foldr (\(x, y) -> insert x y) empty

newtype AList a b =
    AList [(a, b)]
  deriving (Eq, Ord, Read, Show)

instance Injection AList where
  empty = AList []
  insert x y (AList l) = AList ((x, y) : l)
  lookupLeft x (AList l) = fmap snd $ List.find (\(x', _) -> x == x') l
  lookupRight y (AList l) = fmap fst $ List.find (\(_, y') -> y == y') l

data Quadtree a b =
    Empty
  | Branch (a, b) (Quadtree a b) (Quadtree a b) (Quadtree a b) (Quadtree a b)
  deriving (Eq, Ord, Read, Show)

newtype ReadQuadtree a b =
    ReadQuadtree (Quadtree a b)
  deriving (Eq, Ord, Read, Show)

newtype WriteQuadtree a b =
    WriteQuadtree (Quadtree a b)
  deriving (Eq, Ord, Read, Show)

instance Injection ReadQuadtree where
  empty = ReadQuadtree Empty

  insert x y (ReadQuadtree Empty) = ReadQuadtree (Branch (x, y) Empty Empty Empty Empty)
  insert x y (ReadQuadtree (Branch (x', y') t1 t2 t3 t4))
    | x == x' || y == y' = ReadQuadtree (Branch (x, y) t1 t2 t3 t4)
    | otherwise =
        let
          ReadQuadtree t1' = if x < x' then insert x y (ReadQuadtree t1) else ReadQuadtree t1
          ReadQuadtree t2' = if x > x' then insert x y (ReadQuadtree t2) else ReadQuadtree t2
          ReadQuadtree t3' = if y < y' then insert x y (ReadQuadtree t3) else ReadQuadtree t3
          ReadQuadtree t4' = if y > y' then insert x y (ReadQuadtree t4) else ReadQuadtree t4
        in
          ReadQuadtree (Branch (x', y') t1' t2' t3' t4')

  lookupLeft _ (ReadQuadtree Empty) = Nothing
  lookupLeft x (ReadQuadtree (Branch (x', y) t1 t2 t3 t4))
    | x == x' = Just y
    | x < x' = lookupLeft x (ReadQuadtree t1)
    | x > x' = lookupLeft x (ReadQuadtree t2)

  lookupRight _ (ReadQuadtree Empty) = Nothing
  lookupRight y (ReadQuadtree (Branch (x, y') t1 t2 t3 t4))
    | y == y' = Just x
    | y < y' = lookupRight y (ReadQuadtree t3)
    | y > y' = lookupRight y (ReadQuadtree t4)

instance Injection WriteQuadtree where
  empty = WriteQuadtree Empty

  insert x y (WriteQuadtree Empty) = WriteQuadtree (Branch (x, y) Empty Empty Empty Empty)
  insert x y (WriteQuadtree (Branch (x', y') t1 t2 t3 t4))
    | x == x' || y == y' = WriteQuadtree (Branch (x, y) t1 t2 t3 t4)
    | otherwise =
        let
          WriteQuadtree t1' = if x < x' && y < y' then insert x y (WriteQuadtree t1) else WriteQuadtree t1
          WriteQuadtree t2' = if x < x' && y > y' then insert x y (WriteQuadtree t2) else WriteQuadtree t2
          WriteQuadtree t3' = if x > x' && y < y' then insert x y (WriteQuadtree t3) else WriteQuadtree t3
          WriteQuadtree t4' = if x > x' && y > y' then insert x y (WriteQuadtree t4) else WriteQuadtree t4
        in
          WriteQuadtree (Branch (x', y') t1' t2' t3' t4')

  lookupLeft _ (WriteQuadtree Empty) = Nothing
  lookupLeft x (WriteQuadtree (Branch (x', y) t1 t2 t3 t4))
    | x == x' = Just y
    | x < x' = Monad.mplus (lookupLeft x (WriteQuadtree t1)) (lookupLeft x (WriteQuadtree t2))
    | x > x' = Monad.mplus (lookupLeft x (WriteQuadtree t3)) (lookupLeft x (WriteQuadtree t4))

  lookupRight _ (WriteQuadtree Empty) = Nothing
  lookupRight y (WriteQuadtree (Branch (x, y') t1 t2 t3 t4))
    | y == y' = Just x
    | y < y' = Monad.mplus (lookupRight y (WriteQuadtree t1)) (lookupRight y (WriteQuadtree t3))
    | y > y' = Monad.mplus (lookupRight y (WriteQuadtree t2)) (lookupRight y (WriteQuadtree t4))
