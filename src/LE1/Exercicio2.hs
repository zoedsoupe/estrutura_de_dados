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
  , show
  ) where

import Data.List (nub, intercalate)

-- | Implementação

{- | Um TAD Conjunto no qual-}
data Conjunto a = Conjunto [a] deriving (Eq, Read, Ord)

-- | Instancia para ser possivel concatenar Conjuntos
instance Semigroup (Conjunto a) where
  (<>) (Conjunto xs) (Conjunto ys) = Conjunto (xs ++ ys)

-- | Instância para representar um COnjunto vazio
instance Monoid (Conjunto a) where
  mempty = Conjunto []

-- Instancia para imprimir de forma eprsonaliazda um Conjunto
instance Show a => Show (Conjunto a) where
  show (Conjunto []) = "{}"
  show (Conjunto xs) = "{" ++ intercalate ", " (map (show) xs) ++ "}"

-- Inrterface Pública

{- | Converto uma lista de Inteiros
     para um Conjunto de inteiros -}
fromList :: [Integer] -> Conjunto Integer
fromList xs = Conjunto xs

{- | Seguindo a intância da classe Monoid,
     um COnjunto vazio é um TAD com uma lista
     vazia -}
criaConjunto :: Conjunto Integer
criaConjunto = mempty

{- | Insiro um novo elemento num conjunto

     Por questões de eficiência, decidi
     inserir como "head" ou "cabeçalho"
     da lista -}
insereItem :: Integer -> Conjunto Integer -> Conjunto Integer
insereItem x a@(Conjunto ys)
  | not $ pertence x a = Conjunto (x:ys)
  | otherwise          = a

{-  | Removo um item do conjunto de forma recursiva

      O caso base é quando chego no final da lista,
      logo, interrompo a recursão

      Caso seja dado um conjunto válido e um elemento x,
      percorro o conjunto filtrando apenas os elementos
      que são diferentes de x -}
removeItem :: Integer -> Conjunto Integer -> Conjunto Integer
removeItem _ a@(Conjunto []) = a
removeItem x (Conjunto ys)   = Conjunto (filter (/= x) ys)

{- | Dado um Conjunto, verifico se existe pelo menos
     um elemento igual a "x", logo, se existir, "x"
     pertence ao Conjunto -}
pertence :: Integer -> Conjunto Integer -> Bool
pertence x (Conjunto xs) = any (== x) xs
 
{-  | Eu poderia seguir a mesma recursividade
      das funções de "remover" ou "pertence"
      mas decidi usar uma função nativa da linguagem
      "foldl", nesse caso realiza um ação de reduzir
      um conjunto a apenas um elemento "x" onde x é
      o menor elemento do conjunto "xs" -}
minEl :: Conjunto Integer -> Integer
minEl (Conjunto xs) = foldl1 (\acc x -> if x < acc then x else acc) xs

{-  | A união de A e B é definida
      pela junção de todos os elementos de A e B,
      removendo os repetidos

      Para isso, uso a função "nub" que remove os
      elementos duplicados de uma lista e também
      utilizo a função de "remover" como parâmetro
      da função "foldl", que percorre um Traversal
      que neste caso é uma lista, da esquerda para
      a direita reducindo a um acumulador que nessa
      implementação, é uma outra lista

      No final, concateno o Conjunto A com os
      elementos restantes, não repetidos de B -}
uniao :: Conjunto Integer -> Conjunto Integer -> Conjunto Integer
uniao xs (Conjunto [])              = xs
uniao (Conjunto []) ys              = ys
uniao a@(Conjunto xs) (Conjunto ys) =
  (a <> (case xs of
            []      -> nubbed
            (x:xs') -> foldl (flip removeItem) (removeItem x nubbed) xs'))
  where nubbed = Conjunto (nub ys)

{- | Recursivamente percorro o Conjunto A
     verificando se cada elemento de A pertence a B

     Caso seja verdade em todos os casos, retorno True
     senão, False -}
contem :: Conjunto Integer -> Conjunto Integer -> Bool
contem (Conjunto []) _                  = True
contem (Conjunto (x:xs)) b@(Conjunto _) = pertence x b && contem (Conjunto xs) b

{- | Um Conjunto só será estritamente igual a outro
     se os 2 se contem -}
igual :: Conjunto Integer -> Conjunto Integer -> Bool
igual a b = contem a b && contem b a

{- Por "correspondência de valores",
   um conjunto A só será vazio caso
   A = {} -}
isVazio :: Conjunto Integer -> Bool
isVazio (Conjunto []) = True
isVazio _             = False
