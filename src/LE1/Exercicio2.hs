module LE1.Exercicio2 where

import Data.List (nub)

-- Implementação

type ConjuntoInt = [Integer]

-- Interface

criaConjunto :: ConjuntoInt
criaConjunto = []

insereItem :: Integer -> ConjuntoInt -> ConjuntoInt
insereItem x ys = x:ys

removeItem :: Integer -> ConjuntoInt -> ConjuntoInt
removeItem _ [] = []
removeItem x (y:ys)
  | x == y    = removeItem x ys
  | otherwise = y : removeItem x ys

pertence :: Integer -> ConjuntoInt -> Bool
pertence _ [] = False
pertence x (y:ys)
  | x == y    = True
  | otherwise = pertence x ys

min :: ConjuntoInt -> Integer
min xs = foldl1 (\acc x -> if x < acc then x else acc) xs

uniao :: ConjuntoInt -> ConjuntoInt -> ConjuntoInt
uniao xs ys =
  xs
    ++ ( case xs of
           []      -> nub ys
           (x:xs') -> foldl (flip removeItem) (removeItem x (nub ys)) xs'
       )

igual :: ConjuntoInt -> ConjuntoInt -> Bool
igual xs ys = xs == ys

vazio :: ConjuntoInt -> Bool
vazio [] = True
vazio _  = False
