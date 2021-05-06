module Helpers.Stack where

import Helpers
import qualified LE2.Stack.TAD as Stack

introStack :: IO ()
introStack = do
  putStrLn $ toInfo "Iniciando a demonstração do TAD Stack"
  putStrLn $ toInfo "Para essa demonstração, serão apresentados 7 testes interativos que utilizam o TAD!\n" 


getNumbers :: String -> IO [Integer]
getNumbers prompt = go []
  where
    go xs = do
      ans <- promptLine prompt
      case ans of
        "q" -> return xs
        x   -> do
          x' <- return (read x :: Integer)
          putStrLn $ toInfo "Próximo número!"
          go (x':xs)

-- É... Eu sei... Não me julgue...
inversao :: IO ()
inversao = do
  xs <- fmap reverse $ getNumbers "número> "
  let stack = Stack.pushList Stack.new xs
  print Stack.>- stack

runStack :: IO ()
runStack = do
  introStack
  putStrLn $ toInfo "Teste número 1 -> Inversão de dados!"
  putStrLn $ toInfo "Dado uma cadeia de números, o programa imprimirá os elementos na ordem inversa a que foram inseridas"
  putStrLn $ toInfo "Insira um número por linha e insira \"q\" quando quiser prosseguir!\n"
  inversao
