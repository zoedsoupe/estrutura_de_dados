module LE1.Matriz.Array where

import           Data.Array.Unboxed             ( (!)
                                                , UArray
                                                , array
                                                , bounds
                                                , elems
                                                , listArray
                                                , range
                                                )
import           System.Random                  ( mkStdGen
                                                , randomRs
                                                )

type Elem = Int
type Arr = UArray

type Matriz = Arr (Int, Int) Elem

absMatriz :: Matriz -> Matriz
absMatriz a = listArray (bounds a) $ map (abs) xs where xs = elems a

negateMatriz :: Matriz -> Matriz
negateMatriz a = listArray (bounds a) $ map (negate) xs where xs = elems a

fromList :: Int -> Int -> [Elem] -> Matriz
fromList i j = listArray ((0, 0), (i - 1, j - 1))

linhas :: Matriz -> Int
linhas m = numLinhas + 1 where (_, (numLinhas, _)) = bounds m

colunas :: Matriz -> Int
colunas m = numColunas + 1 where (_, (_, numColunas)) = bounds m

matriz :: Int -> Int -> Matriz
matriz m n = fromList m n values'
 where
  format _  0  _  = []
  format m' n' xs = (take m' xs) : format m' (n' - 1) (drop m' xs)
  values  = format m n (take (m * n) (randomRs (3, 10) (mkStdGen (m * n))))
  values' = concat values

transpose :: Matriz -> Matriz
transpose a = array
  (bounds a)
  [ ((linha, coluna), a ! (coluna, linha))
  | linha  <- [sl .. el]
  , coluna <- [sc .. ec]
  ]
  where ((sl, sc), (el, ec)) = bounds a

printMatriz :: Matriz -> IO ()
printMatriz m = putStrLn $ concat
  [ "┌ "
  , unwords (replicate (colunas m) blank)
  , " ┐\n"
  , unlines
    [ "│ " ++ unwords (map (\j -> fill . show $ m ! (i, j)) [0 .. cols]) ++ " │"
    | i <- [0 .. lin]
    ]
  , "└ "
  , unwords (replicate (colunas m) blank)
  , " ┘"
  ]
 where
  xs      = elems m
  strings = map (show) xs
  widest  = maximum $ map (length) strings
  fill str = replicate (widest - length str) ' ' ++ str
  blank = fill ""
  cols  = (colunas m) - 1
  lin   = (linhas m) - 1

zipW :: (a -> b -> c) -> [a] -> [b] -> [c]
zipW _ []       _        = []
zipW _ _        []       = []
zipW f (x : xs) (y : ys) = f x y : zipW f xs ys

somaMatriz :: Matriz -> Matriz -> Matriz
somaMatriz a b | bounds a /= bounds b = array ((0, 0), (-1, 0)) []
               | otherwise            = listArray (bounds a) $ zipW (+) xs ys
 where
  xs = elems a
  ys = elems b

multiplicaMatriz :: Matriz -> Matriz -> Matriz
multiplicaMatriz a b | y0' /= x1' = array ((0, 0), (-1, 0)) []
                     | otherwise  = array ((0, 0), (x0', y1')) resultado
 where
  ((x0, y0), (x0', y0')) = bounds a
  ((_ , y1), (x1', y1')) = bounds b
  linhasA                = range (x0, x0')
  colunasA               = range (y0, y0')
  colunasB               = range (y1, y1')
  resultado =
    [ ((la, cb), sum [ a ! (la, ca) * b ! (ca, cb) | ca <- colunasA ])
    | la <- linhasA
    , cb <- colunasB
    ]
