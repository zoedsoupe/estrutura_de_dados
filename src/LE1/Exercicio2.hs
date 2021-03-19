{-# LANGUAGE DataKinds #-}
module LE1.Exercicio2
  ( criaConjunto
  , insereItem
  , removeItem
  , pertence
  , minEl
  , uniao
  , igual
  , isVazio
  , contem
  , fromList
  ) where

import Data.List (nub, intercalate)

{-| Interface Privada -}

data ConjuntoInt = ConjuntoInt [Integer] deriving (Eq, Read, Ord)

-- Instancia para ser possivel concatenar Conjuntos
instance Semigroup ConjuntoInt where
  (<>) (ConjuntoInt xs) (ConjuntoInt ys) = ConjuntoInt (xs ++ ys)

instance Monoid ConjuntoInt where
  mempty = ConjuntoInt []

-- Instancia para imprimir de forma eprsonaliazda um Conjunto
instance Show ConjuntoInt where
  show (ConjuntoInt []) = "{}"
  show (ConjuntoInt xs) = "{ " ++ intercalate "," (map (show) xs) ++ " }"

-- | Contrato das interfaces
criaConjunto :: ConjuntoInt
fromList :: [Integer] -> ConjuntoInt
insereItem :: Integer -> ConjuntoInt -> ConjuntoInt
removeItem :: Integer -> ConjuntoInt -> ConjuntoInt
minEl :: ConjuntoInt -> Integer
uniao :: ConjuntoInt -> ConjuntoInt -> ConjuntoInt
igual :: ConjuntoInt -> ConjuntoInt -> Bool
contem :: ConjuntoInt -> ConjuntoInt -> Bool
isVazio :: ConjuntoInt -> Bool
pertence :: Integer -> ConjuntoInt -> Bool

-- Inrterface Pública

fromList xs = ConjuntoInt xs

criaConjunto = mempty

{- Insiro um novo elemento num conjunto

   Por questões de eficiência, decidi
   inserir como "head" ou "cabeçalho"
   da lista -}
insereItem x (ConjuntoInt ys)
  | not $ pertence x (ConjuntoInt ys) = ConjuntoInt (x:ys)
  | otherwise                      = ConjuntoInt ys

{-  Removo um item do conjunto de forma recursiva
    O caso base é quando chego no final da lista,
    logo, interrompo a recursão

    Caso seja dado um conjunto válido e um elemento x,
    percorro o conjunto filtrando apenas os elementos
    que são diferentes de x -}
removeItem _ (ConjuntoInt [])     = ConjuntoInt []
removeItem x (ConjuntoInt ys) = ConjuntoInt (filter (/= x) ys)

{- Repito a lógica de remover:
   caso um elemento x seja igual a algum
   "cabeçalho" de um conjunto, retorno True
   case contrário, retorno False

   O caso base da recursão também previne
   argumentos errados -}
pertence x (ConjuntoInt xs) = any (== x) xs
 
{-  Eu poderia seguir a mesma recursividade
    das funções de "remover" ou "pertence"
    mas decidi usar uma função nativa da linguagem
    "foldl", nesse caso realiza um ação de reduzir
    um conjunto a apenas um elemento "x" onde x é
    o menor elemento do conjunto "xs" -}
minEl (ConjuntoInt xs) = foldl1 (\acc x -> if x < acc then x else acc) xs

{-  A união de A e B é definida
    pela junção de todos os elementos de A e B,
    removendo os repetidos

    Para isso, uso a função "nub" que remove os
    elementos duplicados de uma lista e também
    utilizo a função de "remover" como parâmetro
    da função "foldl" -}
uniao xs (ConjuntoInt [])            = xs
uniao (ConjuntoInt []) ys            = ys
uniao (ConjuntoInt xs) (ConjuntoInt ys) =
  (ConjuntoInt xs <> (case xs of
                       []      -> nubbed
                       (x:xs') -> foldl (flip removeItem) (removeItem x nubbed) xs'))
                         where nubbed = ConjuntoInt (nub ys)

contem (ConjuntoInt []) _ = True
contem (ConjuntoInt (x:xs)) (ConjuntoInt ys) = elem x ys && contem (ConjuntoInt xs) (ConjuntoInt ys) 

igual xs ys = contem xs ys && contem ys xs 

{- Por "correspondência de valores",
   um conjunto A só será vazio caso
   A = {} -}
isVazio (ConjuntoInt []) = True
isVazio _                = False
