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
