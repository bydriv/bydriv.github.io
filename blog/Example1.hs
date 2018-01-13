{-# LANGUAGE Rank2Types #-}

example1 :: (forall a. a -> a) -> (Int, Bool)
example1 f = (f 0, f False)
