data Rec a = Rec { unfold :: Rec a -> a }

y :: (a -> a) -> a
y f = unfold (Rec (\x -> f (unfold x x))) (Rec (\x -> f (unfold x x)))
