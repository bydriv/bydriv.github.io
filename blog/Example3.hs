-- 実際にはこのような言語拡張はない
{-# LANGUAGE IntersectionTypes #-}

example3 :: ((Int -> a) /\ (Bool -> b)) -> (a, b)
example3 f = (f 0, f False)
