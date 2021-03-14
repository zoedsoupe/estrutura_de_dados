module LE1.Exercicio2 where

import Data.List (nub)

-- Implementação

type ConjuntoInt = [Integer]

-- Interface

criaConjunto :: ConjuntoInt
criaConjunto = []

{- Insiro um novo elemento num conjunto

   Por questões de eficiência, decidi
   inserir como "head" ou "cabeçalho"
   da lista -}
insereItem :: Integer -> ConjuntoInt -> ConjuntoInt
insereItem x ys = x:ys

{-  Removo um item do conjunto de forma recursiva
    O caso base é quando chego no final da lista,
    logo, interrompo a recursão

    Caso seja dado um conjunto válido e um elemento x,
    percorro o conjunto até achar um "cabeçalho" ou "y"
    que seja igual ao x -}
removeItem :: Integer -> ConjuntoInt -> ConjuntoInt
removeItem _ [] = []
removeItem x (y:ys)
  | x == y    = removeItem x ys
  | otherwise = y : removeItem x ys

{- Repito a lógica de remover:
   caso um elemento x seja igual a algum
   "cabeçalho" de um conjunto, retorno True
   case contrário, retorno False

   O caso base da recursão também previne
   argumentos errados -}
pertence :: Integer -> ConjuntoInt -> Bool
pertence _ [] = False
pertence x (y:ys)
  | x == y    = True
  | otherwise = pertence x ys

{-  Eu poderia seguir a mesma recursividade
    das funções de "remover" ou "pertence"
    mas decidi usar uma função nativa da linguagem
    "foldl", nesse caso realiza um ação de reduzir
    um conjunto a apenas um elemento "x" onde x é
    o menor elemento do conjunto "xs" -}
min :: ConjuntoInt -> Integer
min xs = foldl1 (\acc x -> if x < acc then x else acc) xs

{-  A união de A e B é definida
    pela junção de todos os elementos de A e B,
    removendo os repetidos

    Para isso, uso a função "nub" que remove os
    elementos duplicados de uma lista e também
    utilizo a função de "remover" como parâmetro
    da função "foldl" -}
uniao :: ConjuntoInt -> ConjuntoInt -> ConjuntoInt
uniao xs []  = xs
uniao xs ys =
  xs
    ++ ( case xs of
           []      -> nub ys
           (x:xs') -> foldl (flip removeItem) (removeItem x (nub ys)) xs'
       )

igual :: ConjuntoInt -> ConjuntoInt -> Bool
igual xs ys = xs == ys

{- Por "correspondência de valores",
   um conjunto A só será vazio caso
   A = {} -}
vazio :: ConjuntoInt -> Bool
vazio [] = True
vazio _  = False
