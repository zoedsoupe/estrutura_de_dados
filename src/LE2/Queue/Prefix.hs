module LE2.Queue.Prefix where

import           Debug.Trace                    ( trace )
import qualified LE2.Queue.TAD                 as Queue

data Token
  = Op Char
  | Num Integer
  deriving (Show)

tokenize :: String -> [Token]
tokenize s = go (words s) []
 where
  go []       tokens = reverse tokens

  go (x : xs) tokens = case x of
    "+" -> go xs $ Op '+' : tokens
    "-" -> go xs $ Op '-' : tokens
    "*" -> go xs $ Op '*' : tokens
    "/" -> go xs $ Op '/' : tokens
    _   -> go xs $ Num (read x :: Integer) : tokens

eval :: String -> Integer
eval s = res . go $ fill (tokenize s) Queue.new
 where
  fill []       q = q
  fill (t : ts) q = fill ts $ Queue.enq q t

  go q = undefined

  res q | Queue.size q == 1 = let (Just (Num r), _) = Queue.deq q in r
        | otherwise         = 0

  calc '+' x y = Num $ x + y
  calc '-' x y = Num $ x - y
  calc '*' x y = Num $ x * y
  calc '/' x y = Num $ x `div` y

ehOperador :: Token -> Bool
ehOperador (Op  _) = True
ehOperador (Num _) = False
