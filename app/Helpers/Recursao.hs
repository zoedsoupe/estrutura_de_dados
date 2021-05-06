module Helpers.Recursao
  ( runRecursao
  ) where

import           Control.Monad                  ( when )
import           Helpers
import           LE1.Recursao.Combinacao
import           LE1.Recursao.Raiz

introRecursao :: IO ()
introRecursao = do
  putStrLn $ toInfo "Iniciando demonstração dos algoritmos recursivos..."
  putStrLn
    $ toInfo
        "Nesta demo será mostrado os algoritmos para achar a raiz quadrada com o Método de Newton-Raphson"
  putStrLn
    $ toInfo "E um algoritmo para calcular a combinação entre n e k C(n, k)\n"

runRaiz :: IO ()
runRaiz = do
  putStrLn $ toInfo "Primeiro, o número que você quer calcular a raiz:"
  n <- getDouble
  putStrLn $ toInfo "Agora,diga uma aproximação:"
  a <- getDouble
  putStrLn $ toInfo "Por último, insira uma tolerância para o cálculo:"
  i <- getDouble
  let isNotValid = n < 0 || a < 0 || i <= 0
  when (isNotValid) $ do
    putStrLn $ toFailure "Um dos números é inválido (< 0)... Tente novamente!\n"
    runRaiz
  putStrLn $ toInfo "Calculando..."
  let sqr = nSqrt n a i
  putStrLn
    .  toSuccess
    $  "Raiz de  "
    ++ show n
    ++ " é: "
    ++ (toBold $ show sqr)
    ++ "\n"
  putStrLn $ toInfo "Deseja calcular mais uma raiz? (y/n)"
  res <- askUntil "resposta> " getRes
  if res == "y"
    then runRaiz
    else putStrLn $ toSuccess "Fim demo raiz quadrada!\n"


runCombina :: IO ()
runCombina = do
  putStrLn $ toInfo "Informe o N:"
  n <- getInt
  putStrLn $ toInfo "Informe o K:"
  k <- getInt
  when (n <= 0 || k <= 0) $ do
    putStrLn
      $ toFailure
          "N ou K são negativos ou (== 0)... Impossível calcular a combinação...\n"
    runCombina
  putStrLn $ toInfo "Calculando C(n, k)..."
  let comb = combina n k
  putStrLn
    .  toSuccess
    $  "A combinação entre "
    ++ show n
    ++ " "
    ++ "e "
    ++ show k
    ++ " é: "
    ++ (toBold $ show comb)
    ++ "\n"
  putStrLn $ toInfo "Deseja calcular mais uma combinação? (y/n)"
  res <- askUntil "resposta> " getRes
  if res == "y"
    then runCombina
    else putStrLn $ toSuccess "Fim demo raiz combinação!\n"

runRecursao :: IO ()
runRecursao = do
  introRecursao
  putStrLn . toBold $ toInfo "O primeiro algoritmo é de raiz quadrada!\n"
  runRaiz
  putStrLn . toBold $ toInfo "Agora o algoritmo de combinação!\n"
  runCombina
  putStrLn $ toSuccess "Fim demo algoritmos recursivos!\n"
