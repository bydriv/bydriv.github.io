{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

-- ExistentialQuantification による存在型
data MonoidA = forall m. MonoidA (m, m -> m -> m)

addMonoidA :: MonoidA
addMonoidA = MonoidA (0, (+))

mulMonoidA :: MonoidA
mulMonoidA = MonoidA (1, (*))

strMonoidA :: MonoidA
strMonoidA = MonoidA ("", (++))

-- Rank2Types で次のようにも表現できる
type MonoidB = forall a. (forall m. (m, m -> m -> m) -> a) -> a

addMonoidB :: MonoidB
addMonoidB k = k (0, (+))

mulMonoidB :: MonoidB
mulMonoidB k = k (1, (*))

strMonoidB :: MonoidB
strMonoidB k = k ("", (++))
