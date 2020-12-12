module Data.Injective.Index where

import           Control.Parallel(par, pseq)
import qualified Control.Monad               as Monad

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
        x' = xs !! i
        f = if x < x' then even else odd

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
