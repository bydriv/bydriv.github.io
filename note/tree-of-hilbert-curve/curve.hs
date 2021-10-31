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
