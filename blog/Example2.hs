{-# LANGUAGE Rank2Types #-}

example2 :: forall b. (forall a. a -> b) -> (b, b)
example2 f = (f 0, f False)
