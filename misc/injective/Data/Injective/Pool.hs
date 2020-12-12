{-# LANGUAGE Safe #-}

module Data.Injective.Pool where

import qualified Data.Injective.Map as Map

newtype Pool a =
    Pool (Int, Map.Map Int a)
  deriving (Eq, Ord, Read, Show)

empty :: Pool a
empty = Pool (0, Map.empty)

new :: Ord a => a -> Pool a -> (Int, Pool a)
new x (Pool (i, p)) =
  case Map.lookupRight x p of
    Just (j, _) -> (j, Pool (i, p))
    Nothing -> (i, Pool (i + 1, Map.insert i x p))
