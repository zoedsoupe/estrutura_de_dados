module Helpers.ConjuntoInt (runConjunto) where

import Helpers (toInfo, toSuccess, promptLine, toBold, yellow)
import qualified LE1.ConjuntoInt.TAD as ConjuntoInt

introConjunto :: IO ()
introConjunto = do
  putStrLn $ toInfo "Iniciando demonstração do TAD Conjunto Inteiros"
  putStrLn $ toInfo "Este TAD recebe como entrada uma lista de inteiros\nExemplo: [1,2,3]\n"

getConjunto :: IO String
getConjunto = do
  putStrLn $ toInfo "\nInsira um conjunto de inteiros válido:"
  con <- promptLine "conjunto_int> "
  return con

runConjunto :: IO ()
runConjunto = do
  introConjunto
  vazio <- return ConjuntoInt.criaConjunto
  putStrLn . toSuccess $ "Conjunto criado -> " ++ (show vazio)
  lista <- getConjunto
  putStrLn $ toInfo"\nExecutando \"fromList/1\"\n"  
  let lista'   = read lista :: [Integer]
      conjunto = ConjuntoInt.fromList lista'
  putStrLn . toSuccess $ "Conjunto criado -> " ++ toBold (show conjunto)
  putStrLn $ toInfo "Insira um elemento no conjunto:"
  el <- promptLine "elemento> "
  let conjunto' = ConjuntoInt.insereItem (read el :: Integer) conjunto
  putStrLn . toSuccess $ "\nNovo conjunto -> " ++ toBold (ConjuntoInt.show conjunto')
  putStrLn $ toInfo "Remova um elemento no conjunto:"
  el' <- promptLine "elemento> "
  let conjunto'' = ConjuntoInt.removeItem (read el' :: Integer) conjunto'
  putStrLn . toSuccess $ "\nNovo conjunto -> " ++ toBold (ConjuntoInt.show conjunto'')
  let min1 = ConjuntoInt.minEl conjunto'
  putStrLn . toSuccess $ "Menor elemento do conjunto -> " ++ toBold (show min1)
  putStrLn $ toInfo "Teste se um elemento pertence ao conjunto:"
  el'' <-  promptLine "elemento> "
  let el'''     = read el'' :: Integer
      pertence = if ConjuntoInt.pertence el''' conjunto''
                 then "\nO elemento pertence ao conjunto!"
                 else "\nEste elemento não pertence ao conjunto!"
  putStrLn $ toSuccess pertence
  putStrLn $ toInfo "\nInsira outro conjunto para as compartações"
  lista2 <- getConjunto
  let lista2'   = read lista2 :: [Integer]
      conjunto2 = ConjuntoInt.fromList lista2'
  putStrLn . toSuccess $ "Segundo conjunto -> " ++ toBold (ConjuntoInt.show conjunto2)      
  let min2 = ConjuntoInt.minEl conjunto2
  putStrLn . toSuccess $ "Menor elemento do conjunto -> " ++ toBold (show min2)
  let isVazio = ConjuntoInt.isVazio conjunto'' || ConjuntoInt.isVazio conjunto2
  putStrLn . toInfo $ "Um deles é vazio? " ++ (show isVazio)
  let contem   = ConjuntoInt.contem conjunto'' conjunto2
      contem'  = ConjuntoInt.contem conjunto2 conjunto''
      contem'' = quemContem (contem, contem')
  putStrLn . toInfo $ "Existe intersecção entre eles? " ++ contem''
  let isIguais = ConjuntoInt.igual conjunto'' conjunto2
  putStrLn . toInfo $ "Os conjuntos são iguais? " ++ (show isIguais)
  let uniao = ConjuntoInt.uniao conjunto'' conjunto2
  putStrLn (yellow $ "Conjunto A -> " ++ toBold (show conjunto''))
  putStrLn (yellow $ "Conjunto B -> " ++ toBold (show conjunto2))
  putStrLn $ toInfo "Realizando a união dos conjuntos..."
  putStrLn (yellow "A U B -> " ++ toBold (show uniao) ++ "\n")
  putStrLn $ toSuccess "Fim demo TAD Conjunto Inteiros!\n"  

quemContem :: (Bool, Bool) -> String
quemContem x = case x of
                 (False, False) -> "A e B não se contêm"
                 (True, False)  -> "A ⊃ B"
                 (False, True)  -> "A ⊂ B"
                 (True, True)   -> "Os conjuntos são iguais!"
