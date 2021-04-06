module LE1.Recursao.Combinacao (combina) where

combina :: Int -> Int -> Integer
combina n k
  | k > n || k < 0 = 0
  | otherwise      = constroi n !! n !! k

passo :: [Integer] -> [Integer]
passo xs = 1 : zipWith (+) xs (drop 1 xs) ++ [1]

constroi :: Int -> [[Integer]]
constroi n0 = go (n0 + 1) [1]
  where
    go 0 _ = []
    go n linha = linha : go (n - 1) (passo linha)
