module Data.Injective.Index(
  Index,
  empty,
  singleton,
  insert,
  lookupAt,
  memberAt,
  foldLeft,
  foldRight,
  collectAt,
  fromList,
  toList
) where

import           Control.Parallel(par, pseq)
import qualified Control.Monad               as Monad
import qualified Data.Maybe                  as Maybe

data Index a =
    Empty
  | Branch [a] [Index a]
  deriving (Eq, Ord, Read, Show)

empty :: Index a
empty = Empty

singleton :: [a] -> Index a
singleton xs = Branch xs (replicate (2 ^ length xs) Empty)

insert :: Ord a => [a] -> Index a -> Index a
insert xs Empty = singleton xs
insert xs (Branch xs' indices)
  | length xs /= length xs' =
      error "dimention mismatch"
  | xs == xs' =
      Branch xs indices
  | any (\(x, x') -> x == x') (zip xs xs') =
      error "uniqueness unsatisfied"
  | otherwise =
      let
        conditions =
          mapM (\(x, x') -> [x < x', x > x']) (zip xs xs')

        indices' =
          map
            (\(condition, index) ->
              if and condition then
                insert xs index
              else
                index)
            (zip conditions indices)
      in
        Branch xs' indices'

lookupAt :: Ord a => Int -> a -> Index a -> Maybe [a]
lookupAt _ _ Empty =
  Nothing
lookupAt i x (Branch xs indices)
  | i >= length xs =
      error "dimention mismatch"
  | x == xs !! i =
      Just xs
  | otherwise =
      let
        f = if x < xs !! i then even else odd

        indices' =
          map snd (filter (\(j, _) -> f (j `div` (2 ^ i) `mod` 2)) (zip [0 .. (2 :: Int) ^ length xs - 1] indices))
      in
        lookupAtParallel i x indices'

lookupAtParallel :: Ord a => Int -> a -> [Index a] -> Maybe [a]
lookupAtParallel _ _ [] =
  Nothing
lookupAtParallel i x (index : indices) =
  let
    ~a = lookupAt i x index
    ~b = lookupAtParallel i x indices
  in
    a `par` b `pseq` Monad.mplus a b

memberAt :: Ord a => Int -> a -> Index a -> Bool
memberAt i x = Maybe.isJust . lookupAt i x

foldLeft :: (b -> [a] -> b) -> b -> Index a -> b
foldLeft _ z Empty =
  z
foldLeft f z (Branch xs indices) =
  f (foldl (foldLeft f) z indices) xs

foldRight :: ([a] -> b -> b) -> b -> Index a -> b
foldRight _ z Empty =
  z
foldRight f z (Branch xs indices) =
  foldr (flip (foldRight f)) (f xs z) indices

collectAt :: Int -> Index a -> [a]
collectAt _ Empty =
  []
collectAt i (Branch xs indices) =
  let
    x = xs !! i
  in
    x : collectAtParallel i indices

collectAtParallel :: Int -> [Index a] -> [a]
collectAtParallel _ [] =
  []
collectAtParallel i (index : indices) =
  let
    ~a = collectAt i index
    ~b = collectAtParallel i indices
  in
    a `par` b `pseq` Monad.mplus a b

fromList :: Ord a => [[a]] -> Index a
fromList = foldr insert empty

toList :: Index a -> [[a]]
toList = foldRight (:) []
