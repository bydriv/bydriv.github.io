module Data.Injective.Pool where

import qualified Data.Injective.Injection as Injection

newtype Pool a =
    Pool (Int, Injection.Injection Int a)
  deriving (Eq, Ord, Read, Show)

empty :: Pool a
empty = Pool (0, Injection.empty)

new :: Ord a => a -> Pool a -> (Int, Pool a)
new x (Pool (i, p)) =
  case Injection.lookupRight x p of
    Just j -> (j, Pool (i, p))
    Nothing -> (i, Pool (i + 1, Injection.insert i x p))
