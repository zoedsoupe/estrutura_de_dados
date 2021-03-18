module Helpers (menu) where

-- Imports básicos
import System.Exit (exitSuccess)
import System.Console.Pretty (Pretty(..), Color(..), Style(..), style, color)

-- Imports das funcionalidades/estruturas
import LE1.Exercicio1
import LE1.Exercicio2
import LE1.Exercicio3
import LE1.Exercicio4

menu :: IO ()
menu = do
  putStrLn . unlines $ map concatNums choices
  choice <- getLine
  case validate choice of
    Just _  -> execute . read $ choice
    Nothing -> putStrLn $ toFailure "Opção Inválida!"

  menu
    where concatNums (i, (s, _)) = case s of
                                     "TADs"       -> toBold s
                                     "Estruturas" -> toBold s
                                     "Algoritmos" -> toBold s
                                     _            -> show i ++ ") " ++ s 

validate :: String -> Maybe Int
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length choices)

choices :: [(Int, (String, IO ()))]
choices = zip [0.. ]
  [ ("TADs", undefined)
  , ("Cilindro", undefined)
  , ("Conjunto Inteiro", undefined)
  , ("Data", undefined)
  , ("Clientes", undefined)
  , ("Estruturas", undefined)
  , ("Algoritmos", undefined)
  , ("Sair", exitSuccess)
  ]

execute :: Int -> IO ()
execute n = doExec $ filter (\(i, _) -> i == n) choices
   where doExec ((_, (_,f)):_) = f

-- Funções de ajuda

toBold :: Pretty a => a -> a
toBold s = style Bold s

toSuccess :: Pretty a => a -> a
toSuccess s = color Green s

toFailure :: Pretty a => a -> a
toFailure s = color Red s

toInfo :: Pretty a => a -> a
toInfo s = color Blue s
