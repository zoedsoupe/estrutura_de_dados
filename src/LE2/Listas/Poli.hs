module LE2.Listas.Poli
  ( add
  ) where

import           Data.List                      ( sortBy )
import           Data.Ord                       ( comparing )

type Poly = [(Int, Int)]

sort :: Poly -> Poly
sort = sortBy (comparing $ abs . snd)

sortDesc :: Poly -> Poly
sortDesc = sortBy (flip $ comparing $ abs . snd)

run :: Poly -> Poly -> Poly
run [] ys = ys
run xs [] = xs
run (x@(a, b) : xs) (y@(c, d) : ys) | b == d = (a + c, b) : add xs ys
                                    | b < d  = x : add xs (y : ys)
                                    | b > d  = y : add (x : xs) ys

add :: Poly -> Poly -> Poly
add xs ys = sortDesc $ run (sort xs) (sort ys)
