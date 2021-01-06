module Lemma.Physics where

-- pi ~ pi'
pi' :: Rational
pi' = toRational pi

-- Speed of light
c :: Rational
c = 299792458

-- Planck constant
h :: Rational
h = 6.62607015 / (10 ^ 34)

-- Dirac's constant
h' :: Rational
h' = h / (2 * pi')

-- E = mc^2
emc2 :: Rational -> Rational
emc2 m = m * (c ^ 2)

-- E = pc
epc :: Rational -> Rational -> Rational
epc e v = (e / (c ^ 2)) * v * c

-- E = hv
ehv :: Rational -> Rational
ehv k = h * (c / ((2 * pi') / k))

-- E = kh'c
ekh'c :: Rational -> Rational
ekh'c k = k * h' * c
