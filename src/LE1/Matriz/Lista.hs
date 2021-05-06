{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module LE1.Matriz.Lista where

import           Control.DeepSeq
import           GHC.Generics                   ( Generic
                                                , Generic1
                                                )
import           System.Random                  ( Random
                                                , mkStdGen
                                                , randomRs
                                                )

data Matriz a = M
  { linhas  :: Int
  , colunas :: Int
  , valores :: [[a]]
  }
  deriving (Eq, Ord, Show, Generic, Generic1, NFData, NFData1)

instance Foldable Matriz where
  length (M _ _ xs) = length $ concat xs
  foldMap = undefined
  foldr   = undefined

instance Functor Matriz where
  fmap f (M n m xs) = M n m (map (map f) xs)

instance Num a =>  Num (Matriz a) where
  (+) (M m n xs) (M m' n' ys) | m /= m'   = M 0 0 []
                              | n /= n'   = M 0 0 []
                              | otherwise = M m n (soma xs ys)

  fromInteger = undefined

  signum (M m n _) | m /= n    = M 0 0 []
                   | otherwise = M m n (take m (take m <$> sign))

  abs (M m n xs) = M m n (map (map abs) xs)

  negate (M m n xs) = M m n (map (map negate) xs)

  (*) a@(M _ n _) b@(M m' _ _) | n /= m'   = M 0 0 []
                               | otherwise = multiplica a b

matriz :: (Num a, Random a) => Int -> Int -> Matriz a
matriz m n = M m n values
 where
  format _  0  _  = []
  format m' n' xs = (take m' xs) : format m' (n' - 1) (drop m' xs)
  values = format m n (take (m * n) (randomRs (3, 10) (mkStdGen (m * n))))

transpose :: Num a => Matriz a -> Matriz a
transpose (M m n []              ) = M m n []
transpose (M m n ([]       : xss)) = transpose (M m n xss)
transpose (M m n ((x : xs) : xss)) = M m n (hd : ys)
 where
  hd         = (x : [ h | (h : _) <- xss ])
  (M _ _ ys) = transpose (M m n (xs : [ t | (_ : t) <- xss ]))

printMatriz :: Show a => Matriz a -> IO ()
printMatriz m = putStrLn $ concat
  [ "┌ "
  , unwords (replicate (colunas m) blank)
  , " ┐\n"
  , unlines
    [ "│ "
      ++ unwords (fmap (\j -> fill $ strings ! (i, j)) [1 .. colunas m])
      ++ " │"
    | i <- [1 .. linhas m]
    ]
  , "└ "
  , unwords (replicate (colunas m) blank)
  , " ┘"
  ]
 where
  strings@(M _ _ v) = fmap show m
  widest            = maximum $ fmap length v
  fill str = replicate (widest - length str) ' ' ++ str
  blank = fill ""

sign :: Num a => [[a]]
sign = (1 : repeat 0) : fmap (0 :) sign

(!) :: Matriz a -> (Int, Int) -> a
(!) (M _ n xs) (i, j) = v !! (encode n (i, j)) where v = concat xs

encode :: Int -> (Int, Int) -> Int
encode m (i, j) = (i - 1) * m + j - 1

zipW :: (a -> b -> c) -> [a] -> [b] -> [c]
zipW _ []       _        = []
zipW _ _        []       = []
zipW f (x : xs) (y : ys) = f x y : zipW f xs ys

soma :: Num a => [[a]] -> [[a]] -> [[a]]
soma = (zipW . zipW) (+)

multiplica :: Num a => Matriz a -> Matriz a -> Matriz a
multiplica (M m _ xs) b@(M _ n _) = M m n resultado
 where
  (M _ _ tys) = transpose b
  dot x y = sum $ zipW (*) x y
  resultado = map (\col -> map (dot col) tys) xs
