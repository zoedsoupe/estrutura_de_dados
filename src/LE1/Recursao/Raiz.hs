module LE1.Recursao.Raiz (nSqrt) where

nSqrt :: Double -> Double -> Double -> Double
nSqrt n a i
  | n < 0      = -1
  | a < 0      = -1
  | i <= 0     = -1
  | otherwise  = step i (approximations a n)

approximations :: Double -> Double -> [Double]
approximations x0 n = iterate (prox n) x0

prox :: Double -> Double -> Double
prox n x0 = (x0 + n / x0) / 2

step :: Double -> [Double] -> Double
step _ []  = 0
step _ [_] = 0
step eps (x0:t@(x1:_))
  | abs(x0 - x1) < eps = x1
  | otherwise          = step eps t
