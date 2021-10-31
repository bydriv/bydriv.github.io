module Relation
  ( Hash8(hash8)
  , Relation
  , curve, vertical, horizontal
  , empty, singleton
  , insert, lookupX, lookupY ) where

import qualified Data.Int  as Int
import qualified Data.Word as Word
import qualified Data.List as List
import           Data.Ratio (Ratio, (%), denominator)

class Hash8 a where
  hash8 :: a -> Word.Word8

instance Hash8 Int.Int8 where
  hash8 = fromIntegral

instance Hash8 Word.Word8 where
  hash8 = id

data Relation a b =
    Leaf
  | Branch
      [(a, b)]
      (Ratio Int.Int64)
      (Relation a b)
      (Relation a b)
  deriving (Eq, Ord, Read, Show)

curve :: Integral a => a -> Ratio a -> Ratio a -> Ratio a
curve n x y
  | n <  0                             = error "n must be greater than or equal to 0."
  | x <  0                             = error "x must be greater than or equal to 0."
  | x >= 1                             = error "x must be less than 1."
  | denominator (x / (1 % 2^n)) /= 1   = error "denominator x must be a divisor of 2^n."
  | y <  0                             = error "y must be greater than or equal to 0."
  | y >= 1                             = error "y must be less than 1."
  | denominator (y / (1 % 2^n)) /= 1   = error "denominator y must be a divisor of 2^n."
  | n == 0                             = 0
  | n >  0 && x <  1 % 2 && y <  1 % 2 = curve (n - 1) (y * 2)     (x * 2)           / 4
  | n >  0 && x <  1 % 2 && y >= 1 % 2 = curve (n - 1) (x * 2)     (y * 2 - 1)       / 4 + 1 % 4
  | n >  0 && x >= 1 % 2 && y <  1 % 2 = curve (n - 1) (m - y * 2) (m - (x * 2 - 1)) / 4 + 3 % 4
  | n >  0 && x >= 1 % 2 && y >= 1 % 2 = curve (n - 1) (x * 2 - 1) (y * 2 - 1)       / 4 + 2 % 4
  where m = 1 - 1 % 2^(n-1)

vertical :: Integral a => a -> Ratio a -> Ratio a -> Ratio a -> Bool
vertical n x p q
  | n <  0                                           = error "n must be greater than or equal to 0."
  | x <  0                                           = error "x must be greater than or equal to 0."
  | x >= 1                                           = error "x must be less than 1."
  | denominator (x / (1 % 2^n)) /= 1                 = error "denominator x must be a divisor of 2^n."
  | p <  0                                           = error "p must be greater than or equal to 0."
  | p >= 1                                           = error "p must be less than 1."
  | denominator (p / (1 % 2^(n*2))) /= 1             = error "denominator p must be a divisor of 2^(n*2)."
  | q <  0                                           = error "q must be greater than or equal to 0."
  | q >= 1                                           = error "q must be less than 1."
  | denominator (q / (1 % 2^(n*2))) /= 1             = error "denominator q must be a divisor of 2^(n*2)."
  | p >  q                                           = error "p must be less than or equal to q."
  | p == 0 && q == 1 - 1 % 2^(n*2)                   = True
  | n == 0                                           = True
  | n >  0 && x <  1 % 2 && p >= 0     && q <  1 % 4 = horizontal (n - 1)
                                                         (max 0 (min m (x * 2)))
                                                         (r - max 0 (min r (q * 4)))
                                                         (r - max 0 (min r (p * 4)))
  | n >  0 && x <  1 % 2 && p >= 1 % 4 && q <  1 % 2 = vertical (n - 1)
                                                         (max 0 (min m (x * 2)))
                                                         (max 0 (min r (p * 4 - 1)))
                                                         (max 0 (min r (q * 4 - 1)))
  | n >  0 && x <  1 % 2 && p <  1 % 2 && q <  1     = horizontal (n - 1)
                                                         (max 0 (min m (x * 2)))
                                                         (r - max 0 (min r (q * 4)))
                                                         (r - max 0 (min r (p * 4)))
                                                    || vertical (n - 1)
                                                         (max 0 (min m (x * 2)))
                                                         (max 0 (min r (p * 4 - 1)))
                                                         (max 0 (min r (q * 4 - 1)))
  | n >  0 && x <  1 % 2 && p >= 1 % 2 && q <  1     = False
  | n >  0 && x >= 1 % 2 && p >= 0     && q <  1 % 2 = False
  | n >  0 && x >= 1 % 2 && p >= 3 % 4 && q <  1     = horizontal (n - 1)
                                                         (m - max 0 (min m (x * 2 - 1)))
                                                         (r - max 0 (min r (q * 4 - 3)))
                                                         (r - max 0 (min r (p * 4 - 3)))
  | n >  0 && x >= 1 % 2 && p >= 1 % 2 && q <  3 % 4 = vertical (n - 1)
                                                         (max 0 (min m (x * 2 - 1)))
                                                         (max 0 (min r (p * 4 - 2)))
                                                         (max 0 (min r (q * 4 - 2)))
  | n >  0 && x >= 1 % 2 && p >= 0     && q >= 1 % 2 = vertical (n - 1)
                                                         (max 0 (min m (x * 2 - 1)))
                                                         (max 0 (min r (p * 4 - 2)))
                                                         (max 0 (min r (q * 4 - 2)))
                                                    || horizontal (n - 1)
                                                         (m - max 0 (min m (x * 2 - 1)))
                                                         (r - max 0 (min r (q * 4 - 3)))
                                                         (r - max 0 (min r (p * 4 - 3)))
  where m = 1 - 1 % 2^(n-1)
        r = 1 - 1 % 2^((n-1)*2)

horizontal :: Integral a => a -> Ratio a -> Ratio a -> Ratio a -> Bool
horizontal n y p q
  | n <  0                                           = error "n must be greater than or equal to 0."
  | y <  0                                           = error "y must be greater than or equal to 0."
  | y >= 1                                           = error "y must be less than 1."
  | denominator (y / (1 % 2^n)) /=                   = error "denominator y must be a divisor of 2^n."
  | p <  0                                           = error "p must be greater than or equal to 0."
  | p >= 1                                           = error "p must be less than 1."
  | denominator (p / (1 % 2^(n*2))) /= 1             = error "denominator p must be a divisor of 2^(n*2)."
  | q <  0                                           = error "q must be greater than or equal to 0."
  | q >= 1                                           = error "q must be less than 1."
  | denominator (q / (1 % 2^(n*2))) /= 1             = error "denominator q must be a divisor of 2^(n*2)."
  | p >  q                                           = error "p must be less than or equal to q."
  | p == 0 && q == 1 - 1 % 2^(n*2)                   = True
  | n == 0                                           = True
  | n >  0 && y <  1 % 2 && p >= 1 % 4 && q <  3 % 4 = False
  | n >  0 && y >= 1 % 2 && p >= 3 % 4 && q <  1     = False
  | n >  0 && y >= 1 % 2 && p >= 0     && q <  1 % 4 = False
  | n >  0 && y <  1 % 2 && p >= 0     && q <  3 % 4 = vertical (n - 1)
                                                         (m - max 0 (min m (y * 2)))
                                                         (r - max 0 (min r (q * 4)))
                                                         (r - max 0 (min r (p * 4)))
  | n >  0 && y <  1 % 2 && p >= 1 % 4 && q <  1     = vertical (n - 1)
                                                         (max 0 (min m (y * 2)))
                                                         (r - max 0 (min r (q * 4 - 3)))
                                                         (r - max 0 (min r (p * 4 - 3)))
  | n >  0 && y <  1 % 2 && p <  1 % 4 && q >= 3 % 4 = vertical (n - 1)
                                                         (m - max 0 (min m (y * 2)))
                                                         (r - max 0 (min r (q * 4)))
                                                         (r - max 0 (min r (p * 4)))
                                                    || vertical (n - 1)
                                                         (max 0 (min m (y * 2)))
                                                         (r - max 0 (min r (q * 4 - 3)))
                                                         (r - max 0 (min r (p * 4 - 3)))
  | n >  0 && y >= 1 % 2 && p >= 0     && q <  1 % 2 = horizontal (n - 1)
                                                         (max 0 (min m (y * 2 - 1)))
                                                         (max 0 (min r (p * 4 - 1)))
                                                         (max 0 (min r (q * 4 - 1)))
  | n >  0 && y >= 1 % 2 && p >= 1 % 2 && q <  0     = horizontal (n - 1)
                                                         (max 0 (min m (y * 2 - 1)))
                                                         (max 0 (min r (p * 4 - 2)))
                                                         (max 0 (min r (q * 4 - 2)))
  | n >  0 && y >= 1 % 2 && p <  3 % 4 && q >= 1 % 4 = horizontal (n - 1)
                                                         (max 0 (min m (y * 2 - 1)))
                                                         (max 0 (min r (p * 4 - 1)))
                                                         (max 0 (min r (q * 4 - 1)))
                                                    || horizontal (n - 1)
                                                         (max 0 (min m (y * 2 - 1)))
                                                         (max 0 (min r (p * 4 - 2)))
                                                         (max 0 (min r (q * 4 - 2)))
  where m = 1 - 1 % 2^(n-1)
        r = 1 - 1 % 2^((n-1)*2)

hash :: (Hash8 a, Hash8 b) => a -> b -> Ratio Int.Int64
hash x y =
  curve 8
    (fromIntegral (hash8 x) % 2^8)
    (fromIntegral (hash8 y) % 2^8)

empty :: Relation a b
empty =
  Leaf

singleton :: (Hash8 a, Hash8 b) => a -> b -> Relation a b
singleton x y =
  Branch
    [(x, y)]
    (hash x y)
    Leaf
    Leaf

insert :: (Hash8 a, Hash8 b, Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
insert x y = insert' (hash x y) where
  insert' p Leaf =
    Branch [(x, y)] p Leaf Leaf
  insert' p (Branch xys q l r) =
    case compare p q of
      LT -> Branch xys q (insert' p l) r
      EQ -> Branch (List.insert (x, y) xys) q l r
      GT -> Branch xys q l (insert' p r)

lookupX :: (Hash8 a, Ord a) => a -> Relation a b -> [b]
lookupX = \x r -> lookupX' 0 (1 - 1 % 2^16) x r [] where
  lookupX' _ _ _ Leaf =
    id
  lookupX' p q x (Branch xys z l r) =
    if vertical 8 (fromIntegral (hash8 x) % 2^8 :: Ratio Int.Int64) p q then
      lookupX' p (max p (z - 1 % 2^16)) x l
        . ((map snd (filter (\xy -> x == fst xy) xys)) ++ )
        . lookupX' (min q (z + 1 % 2^16)) q x r
    else
      id

lookupY :: (Hash8 b, Ord b) => b -> Relation a b -> [a]
lookupY = \x r -> lookupY' 0 (1 - 1 % 2^16) x r [] where
  lookupY' _ _ _ Leaf =
    id
  lookupY' p q y (Branch xys z l r) =
    if horizontal 8 (fromIntegral (hash8 y) % 2^8 :: Ratio Int.Int64) p q then
      lookupY' p (max p (z - 1 % 2^16)) y l
        . (map fst (filter (\xy -> y == snd xy) xys) ++)
        . lookupY' (min q (z + 1 % 2^16)) q y r
    else
      id
