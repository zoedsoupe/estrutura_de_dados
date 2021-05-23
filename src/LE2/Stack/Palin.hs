module LE2.Stack.Palin
  ( isPalin
  ) where

import           Data.Char                      ( toLower )
import           Data.List                      ( intercalate )
import qualified LE2.Stack.TAD                 as Stack
import           Text.Deburr                    ( deburr )

parse :: String -> String
parse s =
  deburr $ map toLower [ ch | ch <- s, not (ch `elem` ' ' : ",.?!-:;\"\'") ]

isPalin :: String -> Bool
isPalin s = go (parse s) Stack.new
 where
  go []         st = (parse s) == extract st []
  go (ch : chs) st = go chs $ Stack.push st ch
  extract st xs
    | Stack.isEmpty st = xs
    | otherwise = let (Just c, st') = Stack.pop st in extract st' (xs ++ [c])
