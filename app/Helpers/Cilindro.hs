module Helpers.Cilindro
  ( runCilindro
  ) where

import           Helpers                        ( promptLine
                                                , toBold
                                                , toInfo
                                                , toSuccess
                                                , yellow
                                                )
import qualified LE1.Cilindro.TAD              as Cilindro

introCilindro :: IO ()
introCilindro = do
  putStrLn $ toInfo "Iniciando demonstração do TAD Cilindro"
  putStrLn
    $ toInfo
        "Este TAD recebe como entrada uma 2-tupla (par) de Double, sendo o primeiro elem o raio e o segundo a altura\n"

getCilindro :: IO String
getCilindro = do
  putStrLn $ toInfo "Insira um Cilindro válido:"
  cil <- promptLine "cilindro> "
  return cil

runCilindro :: IO ()
runCilindro = do
  introCilindro
  tuple <- getCilindro
  putStrLn $ toInfo "Executando \"fromTuple/1\""
  c <- return $ Cilindro.fromTuple (read tuple :: (Double, Double))
  putStrLn . toSuccess $ "\nTAD == " ++ (show c) ++ "\n"
  let isVazio = Cilindro.isVazio c
  putStrLn . toInfo $ "Esse Cilindro é vazio? " ++ (show isVazio)
  putStrLn $ toInfo "\nE qual a altura desse Cilindro?"
  let altura = Cilindro.getAltura c
  putStrLn (yellow "Altura -> " ++ toBold (show altura))
  putStrLn $ toInfo "\nJá o raio deste Cilindro é:"
  let raio = Cilindro.getRaio c
  putStrLn (yellow "Raio -> " ++ toBold (show raio))
  putStrLn $ toInfo "\nCalculando a área do Cilindro..."
  let area = Cilindro.calcArea c
  putStrLn (yellow "Area -> " ++ toBold (show area))
  putStrLn $ toInfo "\nPor último, vamos ver seu volume:"
  let volume = Cilindro.calcVolume c
  putStrLn (yellow "Volume -> " ++ toBold (show volume) ++ "\n")
  putStrLn $ toSuccess "Fim demo TAD Cilindro!\n"
