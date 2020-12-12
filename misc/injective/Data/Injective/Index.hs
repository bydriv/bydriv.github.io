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
  | otherwise =
      let
        values =
          map (\(x, x') -> [(x, x'), (x', x)]) (zip xs xs')

        combinations =
          Monad.foldM (\ys' ys -> do { y <- ys; return (y : ys') }) [] values

        indices' =
          map
            (\(combination, index) ->
              if all (\(x, x') -> x < x') combination then
                insert xs index
              else
                index)
            (zip combinations indices)
      in
        Branch xs' indices'

lookupAt :: Ord a => Int -> a -> Index a -> Maybe [a]
lookupAt _ _ Empty =
  Nothing
lookupAt i x (Branch xs indices)
  | i >= length xs =
      error "dimention mismatch"
  | otherwise =
      let
        x' = xs !! i
      in
        if x == x' then
          Just xs
        else if x < x' then
          let
            n :: Int
            n = 2 ^ length xs

            indices' =
              map snd (filter (\(j, _) -> even (j `div` (2 ^ i) `mod` 2)) (zip [0 .. n - 1] indices))

            lookupParallel [] =
              Nothing
            lookupParallel (index : indices'') =
              let
                ~r1 = lookupAt i x index
                ~r2 = lookupParallel indices''
              in
                r1 `par` r2 `pseq` Monad.mplus r1 r2
          in
            lookupParallel indices'
        else if x > x' then
          let
            n :: Int
            n = 2 ^ length xs

            indices' =
              map snd (filter (\(j, _) -> odd (j `div` (2 ^ i) `mod` 2)) (zip [0 .. n - 1] indices))

            lookupParallel [] =
              Nothing
            lookupParallel (index : indices'') =
              let
                ~r1 = lookupAt i x index
                ~r2 = lookupParallel indices''
              in
                r1 `par` r2 `pseq` Monad.mplus r1 r2
          in
            lookupParallel indices'
        else
          undefined
