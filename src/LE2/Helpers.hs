module LE2.Helpers
  ( randList
  , randMatrix
  ) where

import System.Random (Random)
import Data.Random.Normal (mkNormals')

seed :: Num a => a
seed = 726420620519930218067873149734376981

randList :: (Random a, Floating a, RealFrac a, Integral b) => (a, a) -> Int -> [b]
randList (m, sd) n = map (round) $ take n $ mkNormals' (m, sd) seed

randMatrix :: (Random a, Floating a, RealFrac a) => (a, a) -> (Int, Int) -> [[Int]]
randMatrix (m, sd) (a, b)
  | a /= b    = []
  | otherwise = [ randList (m, sd) a | _ <- [1..b] ]
