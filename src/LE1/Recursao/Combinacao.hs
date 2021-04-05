module LE1.Recursao.Combinacao where

combina :: Integral a => a -> a -> a
combina n k | k == 0         = 1
            | n == k         = 1
combina n k | k > 0 && n > k = (combina (n - 1) k) + (combina (n - 1) (k - 1))
combina _ _                  = 0
