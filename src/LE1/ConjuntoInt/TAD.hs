{-# LANGUAGE DataKinds #-}
module LE1.ConjuntoInt.TAD
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

import Data.List (intercalate)

-- | Implementação
data Conjunto a = Conjunto [a] deriving (Eq, Read, Ord)

instance Semigroup (Conjunto a) where
  (<>) (Conjunto xs) (Conjunto ys) = Conjunto (xs ++ ys)

instance Monoid (Conjunto a) where
  mempty = Conjunto []

instance Show a => Show (Conjunto a) where
  show (Conjunto []) = "∅"
  show (Conjunto xs) = "{" ++ intercalate ", " (map (show) xs) ++ "}"

-- Inrterface Pública

fromList :: [Integer] -> Conjunto Integer
fromList xs = Conjunto (nub xs)

criaConjunto :: Conjunto Integer
criaConjunto = mempty

insereItem :: Integer -> Conjunto Integer -> Conjunto Integer
insereItem x a@(Conjunto ys)
  | not $ pertence x a = Conjunto (x:ys)
  | otherwise          = a

removeItem :: Integer -> Conjunto Integer -> Conjunto Integer
removeItem _ a@(Conjunto []) = a
removeItem x (Conjunto ys)   = Conjunto (filter (/= x) ys)

pertence :: Integer -> Conjunto Integer -> Bool
pertence x (Conjunto xs) = any (== x) xs
 
minEl :: Conjunto Integer -> Integer
minEl (Conjunto xs) = foldl1 (\acc x -> if x < acc then x else acc) xs

uniao :: Conjunto Integer -> Conjunto Integer -> Conjunto Integer
uniao xs (Conjunto [])              = xs
uniao (Conjunto []) ys              = ys
uniao a@(Conjunto xs) (Conjunto ys) =
  (a <> (case xs of
            []      -> nubbed
            (x:xs') -> foldl (flip removeItem) (removeItem x nubbed) xs'))
  where nubbed = Conjunto (nub ys)

contem :: Conjunto Integer -> Conjunto Integer -> Bool
contem (Conjunto []) _                  = True
contem (Conjunto (x:xs)) b@(Conjunto _) = pertence x b && contem (Conjunto xs) b

igual :: Conjunto Integer -> Conjunto Integer -> Bool
igual a b = contem a b && contem b a

isVazio :: Conjunto Integer -> Bool
isVazio (Conjunto []) = True
isVazio _             = False

-- | Funções de ajuda
nub :: Eq a => [a] -> [a]
nub = foldl (\seen x -> if elem x seen
                        then seen
                        else seen ++ [x]) []
