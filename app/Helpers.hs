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
  , ("Conjunto Inteiro", runConjunto)
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

-- | Conjunto de inteiros

introConjunto :: IO ()
introConjunto = do
  putStrLn $ toInfo "Iniciando demonstração do TAD Conjunto Inteiros"
  putStrLn $ toInfo "Este TAD recebe como entrada uma lista de inteiros\n"

getConjunto :: IO String
getConjunto = do
  putStrLn $ toInfo"\nExecutando \"fromList/1\"\n"
  cil <- promptLine "conjunto_int> "
  return cil

runConjunto :: IO ()
runConjunto = do
  introConjunto
  vazio <- return ConjuntoInt.criaConjunto
  putStrLn . toSuccess $ "Conjunto criado -> " ++ (show vazio)
  lista <- getConjunto
  let lista'   = read lista :: [Integer]
      conjunto = ConjuntoInt.fromList lista'
  putStrLn . toSuccess $ "Conjunto criado -> " ++ toBold (show conjunto)
  putStrLn $ toInfo "Insira um elemento no conjunto:"
  el <- promptLine "elemento> "
  let conjunto' = ConjuntoInt.insereItem (read el :: Integer) conjunto
  putStrLn . toSuccess $ "Novo conjunto -> " ++ toBold (ConjuntoInt.show conjunto')
  putStrLn $ toInfo "Remova um elemento no conjunto:"
  el <- promptLine "elemento> "
  let conjunto'' = ConjuntoInt.removeItem (read el :: Integer) conjunto'
  putStrLn . toSuccess $ "Novo conjunto -> " ++ toBold (ConjuntoInt.show conjunto'')
  let min1 = ConjuntoInt.minEl conjunto'
  putStrLn . toSuccess $ "Menor elemento do conjunto -> " ++ toBold (show min1)
  putStrLn $ toInfo "Teste se um elemento pertence ao conjunto:"
  el' <-  promptLine "elemento> "
  let el''     = read el' :: Integer
      pertence = if ConjuntoInt.pertence el'' conjunto'' then "O elemento pertence ao conjunto!" else "Este elemento não pertence ao conjunto!"
  putStrLn $ toSuccess pertence
  putStrLn $ toInfo "Insira outro conjunto para as compartações"
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
