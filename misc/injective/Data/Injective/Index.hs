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
        Monad.msum (map (lookupAt i x) indices')

memberAt :: Ord a => Int -> a -> Index a -> Bool
memberAt i x = Maybe.isJust . lookupAt i x

foldLeft :: (b -> [a] -> b) -> b -> Index a -> b
foldLeft _ z Empty =
  z
foldLeft f z (Branch xs indices) =
  let
    (indices1, indices2) = splitAt (2 ^ (length xs - 1)) indices
  in
    foldl (foldLeft f) (f (foldl (foldLeft f) z indices1) xs) indices2

foldRight :: ([a] -> b -> b) -> b -> Index a -> b
foldRight _ z Empty =
  z
foldRight f z (Branch xs indices) =
  let
    (indices1, indices2) = splitAt (2 ^ (length xs - 1)) indices
  in
    foldr (flip (foldRight f)) (f xs (foldr (flip (foldRight f)) z indices2)) indices1

collectAt :: Int -> Index a -> [a]
collectAt _ Empty =
  []
collectAt i (Branch xs indices) =
  let
    x = xs !! i
  in
    x : Monad.msum (map (collectAt i) indices)

fromList :: Ord a => [[a]] -> Index a
fromList = foldr insert empty

toList :: Index a -> [[a]]
toList = foldRight (:) []
