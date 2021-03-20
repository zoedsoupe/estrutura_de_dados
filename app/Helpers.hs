module Helpers (menu) where

-- Imports básicos
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import System.Console.Pretty (Pretty(..), Color(..), Style(..), style, color)

-- Imports das funcionalidades/estruturas
import qualified LE1.Exercicio1 as Cilindro
import qualified LE1.Exercicio2 as ConjuntoInt
import qualified LE1.Exercicio3 as Data
import qualified LE1.Exercicio4 as Clientes

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
  , ("Conjunto Inteiro", undefined)
  , ("Data", undefined)
  , ("Clientes", undefined)
  , ("Estruturas", menu)
  , ("Algoritmos", menu)
  , ("Sair", exit)
  ]

execute :: Int -> IO ()
execute n = doExec $ filter (\(i, _) -> i == n) choices
   where doExec ((_, (_,f)):_) = f

-- Funções de ajuda

exit :: IO ()
exit = do
  putStrLn $ toSuccess "Até mais!!!"
  exitSuccess

toBold :: Pretty a => a -> a
toBold s = style Bold s

toSuccess :: Pretty a => a -> a
toSuccess s = color Green s

toFailure :: Pretty a => a -> a
toFailure s = color Red s

toInfo :: Pretty a => a -> a
toInfo s = color Blue s

yellow :: Pretty a => a -> a
yellow s = color Yellow s

promptLine :: String -> IO String
promptLine prompt = do
  putStr $ color Yellow prompt
  hFlush stdout
  getLine

{- | Funções de entrada do programa -}

-- | Cilindro

introCilindro :: IO ()
introCilindro = do
  putStrLn $ toInfo "Iniciando demonstração do TAD Cilindro"
  putStrLn $ toInfo "Este TAD recebe como entrada uma 2-tupla (par) de Double, sendo o primeiro el o raio e o segundo a altura\n"

getCilindro :: IO String
getCilindro = do
  putStrLn $ toInfo"\nExecutando \"fromTuple/1\"\n"
  cil <- promptLine "cilindro> "
  return cil
  
runCilindro :: IO ()
runCilindro = do
  introCilindro
  tuple <- getCilindro
  c <- return $ Cilindro.fromTuple (read tuple :: (Double, Double))
  putStrLn . toSuccess $ "\nTAD == " ++ (show c) ++ "\n"
  let isVazio = Cilindro.isVazio c
  putStrLn . toInfo $ "Esse Cilindro é vazio? " ++ (show isVazio)
  let isValido = Cilindro.valido c
  putStrLn . toInfo $ "Será que esse Cilindro é válido?: " ++ (show isValido)
  putStrLn $ toInfo "E qual a altura desse Cilindro?"
  let altura = Cilindro.getAltura c
  putStrLn (yellow "Altura -> " ++ toBold (show altura))
  putStrLn $ toInfo "Já o raio deste Cilindro é:"
  let raio = Cilindro.getRaio c
  putStrLn (yellow "Raio -> " ++ toBold (show raio))
  putStrLn $ toInfo "Calculando a área do Cilindro..."
  let area = Cilindro.calcArea c
  putStrLn (yellow "Area -> " ++ toBold (show area))
  putStrLn $ toInfo "Por último, vamos ver seu volume:"
  let volume = Cilindro.calcVolume c
  putStrLn (yellow "Volume -> " ++ toBold (show volume) ++ "\n")
  putStrLn $ toSuccess "Fim demo TAD Cilindro!\n"
