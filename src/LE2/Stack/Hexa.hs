module LE2.Stack.Hexa ( hexarize ) where

import           Data.Char                      ( chr )
import qualified LE2.Stack.TAD                 as Stack

zeroASCII :: Int
zeroASCII = 48

intToDigit :: Int -> Char
intToDigit 10 = 'A'
intToDigit 11 = 'B'
intToDigit 12 = 'C'
intToDigit 13 = 'D'
intToDigit 14 = 'E'
intToDigit 15 = 'F'
intToDigit x  = chr $ x + zeroASCII

hexarize :: Int -> String
hexarize n = go n Stack.new
 where
  go x s
    | x < 16
    = [intToDigit x] ++ (reverse $ (Stack.<<>) s)
    | otherwise
    = let (q, r) = x `divMod` 16 in go q $ Stack.push s (intToDigit r)
