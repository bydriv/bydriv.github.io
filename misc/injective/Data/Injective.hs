module Data.Injective where

import           Control.Parallel(par, pseq)
import qualified Control.Monad               as Monad
import qualified Data.Maybe                  as Maybe

data Injection a b =
    Empty
  | Branch a b (Injection a b) (Injection a b) (Injection a b) (Injection a b)
  deriving (Eq, Ord, Read, Show)

empty :: Injection a b
empty = Empty

singleton :: a -> b -> Injection a b
singleton x y = Branch x y Empty Empty Empty Empty

insert :: (Ord a, Ord b) => a -> b -> Injection a b -> Injection a b
insert x y Empty = Branch x y Empty Empty Empty Empty
insert x y (Branch x' y' t1 t2 t3 t4)
  | x == x' || y == y' = Branch x y t1 t2 t3 t4
  | otherwise =
      let
        t1' = if x < x' && y < y' then insert x y t1 else t1
        t2' = if x < x' && y > y' then insert x y t2 else t2
        t3' = if x > x' && y < y' then insert x y t3 else t3
        t4' = if x > x' && y > y' then insert x y t4 else t4
      in
        Branch x' y' t1' t2' t3' t4'

lookupLeft :: (Ord a, Ord b) => a -> Injection a b -> Maybe b
lookupLeft _ Empty = Nothing
lookupLeft x (Branch x' y t1 t2 t3 t4)
  | x == x' = Just y
  | x < x' =
      let
        a = lookupLeft x t1
        b = lookupLeft x t2
      in
        a `par` b `pseq` Monad.mplus a b
  | x > x' =
      let
        a = lookupLeft x t3
        b = lookupLeft x t4
      in
        a `par` b `pseq` Monad.mplus a b
  | otherwise = undefined

lookupRight :: (Ord a, Ord b) => b -> Injection a b -> Maybe a
lookupRight _ Empty = Nothing
lookupRight y (Branch x y' t1 t2 t3 t4)
  | y == y' = Just x
  | y < y' =
      let
        ~a = lookupRight y t1
        ~b = lookupRight y t3
      in
        a `par` b `pseq` Monad.mplus a b
  | y > y' =
      let
        ~a = lookupRight y t2
        ~b = lookupRight y t4
      in
        a `par` b `pseq` Monad.mplus a b
  | otherwise = undefined

memberLeft :: (Ord a, Ord b) => a -> Injection a b -> Bool
memberLeft x f = Maybe.isJust (lookupLeft x f)

memberRight :: (Ord a, Ord b) => b -> Injection a b -> Bool
memberRight y f = Maybe.isJust (lookupRight y f)

fromAList :: (Ord a, Ord b) => [(a, b)] -> Injection a b
fromAList = foldr (\(x, y) -> insert x y) empty
