data Rec a = Empty a | Rec (Rec a -> a)

unfold :: Rec a -> Rec a -> a
unfold (Empty x) = const x
unfold (Rec f) = f

y' :: (a -> a) -> a
y' f = unfold (Rec (\x -> f (unfold x x))) (Rec (\x -> f (unfold x x)))
