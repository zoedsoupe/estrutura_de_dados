module LE2.Queue.Category where

import           Data.Char                      ( toLower )
import qualified LE2.Queue.TAD                 as Queue

split :: String -> (Integer, Integer)
split s = go (words s) Queue.new Queue.new
 where
  go [] q1 q2 = (Queue.size q1, Queue.size q2)

  go (w@(ch : _) : xs) q1 q2 | ehVogal ch = go xs (Queue.enq q1 w) q2
                             | otherwise  = go xs q1 (Queue.enq q2 w)

ehVogal :: Char -> Bool
ehVogal ch | toLower ch `elem` "aeiou" = True
ehVogal _                              = False
