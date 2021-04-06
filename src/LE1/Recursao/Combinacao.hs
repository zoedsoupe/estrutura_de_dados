module LE1.Recursao.Combinacao (combina) where

combina :: Integral a => a -> a -> a
combina n k0 = combina' 1 1
  where
    combina' acc k
      | k > k0    = seq n acc
      | otherwise = combina' ((n - k + 1) * acc `quot` k) (k + 1)
