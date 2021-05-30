module LE2.Stack.Delim
  ( parse
  ) where

import qualified LE2.Stack.TAD                 as Stack

erroParen :: String
erroParen = "Erro: fecha parentêses não casa!"

erroCol :: String
erroCol = "Erro: fecha colchetes não casa!"

erroChaves :: String
erroChaves = "Erro: fecha chaves não casa!"

parse :: String -> [String]
parse lines = go lines Stack.new []
 where
  go "" st parsed | Stack.isEmpty st = parsed
                  | otherwise        = map apply (extract st) ++ parsed

  go ('(' : chs) st parsed = go chs (Stack.push st '(') parsed
  go ('[' : chs) st parsed = go chs (Stack.push st '[') parsed
  go ('{' : chs) st parsed = go chs (Stack.push st '{') parsed

  go (')' : chs) st parsed
    | Stack.peek st == Just '('
    = let (Just _, st') = Stack.pop st in go chs st' parsed
    | otherwise
    = go chs st $ erroParen : parsed

  go (']' : chs) st parsed
    | Stack.peek st == Just '['
    = let (Just _, st') = Stack.pop st in go chs st' parsed
    | otherwise
    = go chs st $ erroCol : parsed

  go ('}' : chs) st parsed
    | Stack.peek st == Just '{'
    = let (Just _, st') = Stack.pop st in go chs st' parsed
    | otherwise
    = go chs st $ erroChaves : parsed

  go (ch : chs) st parsed = go chs st parsed

  extract st = reverse $ Stack.(<<>) st

  apply '(' = erroParen
  apply '[' = erroCol
  apply '{' = erroChaves
