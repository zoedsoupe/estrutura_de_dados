module LE1.Helpers
  ( Matrix(..)
  , show
  , randMatrix
  ) where

import System.Random (Random, randomRs, mkStdGen)

data Matrix a = Matrix { linhas  :: Int
                       , colunas :: Int
                       , valores :: [[a]]
                       } deriving (Eq, Ord)

instance Show m => Show (Matrix m) where
  show (Matrix _ _ []) = "[]"
  show (Matrix _ _ vs) = showLines vs
    where showLines [] = ""
          showLines (x:xs) = showElems x ++ "\n" ++ showLines xs
          showElems [] = ""
          showElems (x:xs) = show x ++ "  " ++ showElems xs

randMatrix :: (Num a, Random a) => Int -> Matrix a
randMatrix n = Matrix n n values
  where format _ 0 _   = []
        format n' m xs = (take n' xs) : format n' (m-1) (drop n' xs)
        values         = format n n (take (n*n) (randomRs (3, 10) (mkStdGen (n*n))))
