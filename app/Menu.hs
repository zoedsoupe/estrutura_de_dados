module Menu (menu) where

import Helpers (toBold, toFailure, exit)

{- | Imports das funcionalidades/estruturas -}

-- | TADs
import Helpers.Cilindro
import Helpers.ConjuntoInt
import Helpers.Data
import Helpers.Clientes

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
  [ ("TADs", menu)
  , ("Cilindro", runCilindro)
  , ("Conjunto Inteiro", runConjunto)
  , ("Data", runData)
  , ("Clientes", runClientes)
  , ("Estruturas", menu)
  , ("Algoritmos", menu)
  , ("Sair", exit)
  ]

execute :: Int -> IO ()
execute n = doExec $ filter (\(i, _) -> i == n) choices
   where doExec ((_, (_,f)):_) = f
