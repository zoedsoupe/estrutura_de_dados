module LE1.Exercicio5 where

import LE1.Helpers (Matrix(..))

somaMatriz :: Num a => Matrix a -> Matrix a -> Matrix a
somaMatriz (Matrix _ _ [[]]) (Matrix _ _ [[]]) = Matrix 0 0 []
somaMatriz a@(Matrix _ _ (x:xs)) b@(Matrix _ _ (y:ys))
  | linhas a /= linhas b   = Matrix 0 0 []
  | colunas a /= colunas b = Matrix 0 0 []
  | otherwise              = c
    where a' = Matrix (linhas a) (colunas a) xs
          b' = Matrix (linhas b) (colunas b) ys
          cs = (zipWith (+) x y : valores (somaMatriz a' b'))
          c  = Matrix (linhas a) (colunas a) cs
