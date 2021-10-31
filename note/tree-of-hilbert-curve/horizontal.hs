horizontal :: Integral a => a -> Ratio a -> Ratio a -> Ratio a -> Bool
horizontal n y p q
  | n <  0                                           = error "n must be greater than or equal to 0."
  | y <  0                                           = error "y must be greater than or equal to 0."
  | y >= 1                                           = error "y must be less than 1."
  | denominator (y / (1 % 2^n)) /= 1                 = error "denominator y must be a divisor of 2^n."
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
