module Helpers.Data
  ( runData
  ) where

import           Helpers
import qualified LE1.Data.TAD                  as Data

introData :: IO ()
introData = do
  putStrLn $ toInfo "Iniciando demonstração do TAD Data"
  putStrLn
    $ toInfo
        "Este TAD recebe como entrada uma 3-tupla de Int, que corresponde ao formato DD/MM/AAAA\n"

validData :: String -> Either String String
validData s = do
  let d = Data.fromTuple (read s :: (Int, Int, Int))
  if Data.isInvalida d
    then Left (toFailure "Data Inválida, tente novamente\n")
    else Right s

getData :: IO String
getData = do
  dt <- promptLine "data> "
  return dt

runData :: IO ()
runData = do
  introData
  putStrLn $ toInfo "Insira uma data: "
  d <- askUntil "data> " validData
  print d
  d' <- Data.imprimeData (read d :: (Int, Int, Int))
  putStrLn $ toSuccess "Data criada -> " ++ toBold (show d')
  putStrLn $ toInfo "Insira uma data no formato DD/MM/AAAA:"
  d'' <- getData
  putStrLn
    $ toInfo "Convertendo a string num TAD Data com os seguintes argumentos -> "
    ++ d''
    ++ " "
    ++ (show Data.vazia)
  let cData = Data.converteData d'' Data.vazia
  putStrLn $ toSuccess "Data convertida -> " ++ toBold (show cData)
  let iData   = "27/07/2021"
      sData   = Data.somaDias (Data.converteData iData Data.vazia) 5
      sData'  = Data.somaDias (Data.converteData iData Data.vazia) 43
      iData'  = "31/12/2020"
      sData'' = Data.somaDias (Data.converteData iData' Data.vazia) 10
  putStrLn $ toInfo "\nExecutando função somaDias/2...\n"
  putStrLn (yellow (iData ++ " + 5 dias -> ") ++ toBold (Data.show sData))
  putStrLn (yellow (iData ++ " + 43 dias -> ") ++ toBold (Data.show sData'))
  putStrLn (yellow (iData' ++ " + 10 dias -> ") ++ toBold (Data.show sData''))
  putStrLn $ toSuccess "\nFim demo TAD Data!\n"
