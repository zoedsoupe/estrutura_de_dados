module LE2.Stack.Delim
  ( parse
  , erroAbreParen
  , erroAbreCol
  , erroAbreChaves
  , erroFechaParen
  , erroFechaCol
  , erroFechaChaves
  ) where

import           GHC.Exts                       ( sortWith )
import qualified LE2.Stack.TAD                 as Stack

import           Debug.Trace                    ( trace )

tokenize :: String -> [(Char, Int)]
tokenize s = zip (filter ehDelim s) [0 ..]

parse :: String -> [String]
parse lines = go (tokenize lines) Stack.new []
 where
  go x y z | trace ("go: " ++ show x ++ " " ++ show y ++ " " ++ show z) False =
    undefined
  go [] st parsed
    | Stack.isEmpty st = map fst $ sort parsed
    | otherwise        = map fst . sort $ parsed ++ map swap ((Stack.<<>) st)

  go (('(', idx) : xs) st parsed = go xs (Stack.push st (')', idx)) parsed
  go (('[', idx) : xs) st parsed = go xs (Stack.push st (']', idx)) parsed
  go (('{', idx) : xs) st parsed = go xs (Stack.push st ('}', idx)) parsed

  go (pair : xs) st parsed | Stack.isEmpty st = go xs st $ apply pair : parsed

  go ((')', _) : xs) st parsed | Just (')', _) <- Stack.peek st =
    let (Just _, st') = Stack.pop st in go xs st' parsed

  go ((']', _) : xs) st parsed | Just (']', _) <- Stack.peek st =
    let (Just _, st') = Stack.pop st in go xs st' parsed

  go (('}', _) : xs) st parsed | Just ('}', _) <- Stack.peek st =
    let (Just _, st') = Stack.pop st in go xs st' parsed

  go (pair : xs) st parsed =
    let (_, st') = Stack.pop st in go (pair : xs) st' $ apply pair : parsed

  swap ('(', idx) = apply (')', idx)
  swap (')', idx) = apply ('(', idx)
  swap ('[', idx) = apply (']', idx)
  swap (']', idx) = apply ('[', idx)
  swap ('{', idx) = apply ('}', idx)
  swap ('}', idx) = apply ('{', idx)

  apply ('(', idx) = (erroAbreParen, idx)
  apply (')', idx) = (erroFechaParen, idx)
  apply ('[', idx) = (erroAbreCol, idx)
  apply (']', idx) = (erroFechaCol, idx)
  apply ('{', idx) = (erroAbreChaves, idx)
  apply ('}', idx) = (erroFechaChaves, idx)

  sort = sortWith (abs . snd)

erroFechaParen :: String
erroFechaParen = "Erro: fecha parentêses não casa!"

erroFechaCol :: String
erroFechaCol = "Erro: fecha colchetes não casa!"

erroFechaChaves :: String
erroFechaChaves = "Erro: fecha chaves não casa!"

erroAbreParen :: String
erroAbreParen = "Erro: abre parentêses não casa!"

erroAbreCol :: String
erroAbreCol = "Erro: abre colchetes não casa!"

erroAbreChaves :: String
erroAbreChaves = "Erro: abre chaves não casa!"

ehDelim :: Char -> Bool
ehDelim ch | elem ch "()[]{}" = True
           | otherwise        = False
