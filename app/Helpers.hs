module Helpers (menu) where

-- Imports básicos
import Data.Decimal (Decimal)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import System.Console.Pretty (Pretty(..), Color(..), Style(..), style, color)
import Text.Layout.Table (HeaderSpec, ColSpec, tableString, def, numCol, unicodeRoundS, rowG, titlesH)

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
  , ("Data", runData)
  , ("Clientes", runClientes)
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

askUntil :: String -> (String -> (Either String String)) -> IO String
askUntil prompt confirm = go
  where
    go = do
      answer <- promptLine prompt
      answer' <- return $ confirm answer
      case answer' of
        Left msg -> putStr msg >> go 
        Right res -> pure res

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
  putStrLn $ toInfo "Este TAD recebe como entrada uma lista de inteiros\nExemplo: [1,2,3]\n"

getConjunto :: IO String
getConjunto = do
  putStrLn $ toInfo"\nExecutando \"fromList/1\"\n"
  con <- promptLine "conjunto_int> "
  return con

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
  el' <- promptLine "elemento> "
  let conjunto'' = ConjuntoInt.removeItem (read el' :: Integer) conjunto'
  putStrLn . toSuccess $ "Novo conjunto -> " ++ toBold (ConjuntoInt.show conjunto'')
  let min1 = ConjuntoInt.minEl conjunto'
  putStrLn . toSuccess $ "Menor elemento do conjunto -> " ++ toBold (show min1)
  putStrLn $ toInfo "Teste se um elemento pertence ao conjunto:"
  el'' <-  promptLine "elemento> "
  let el'''     = read el'' :: Integer
      pertence = if ConjuntoInt.pertence el''' conjunto'' then "O elemento pertence ao conjunto!" else "Este elemento não pertence ao conjunto!"
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

-- | Data

introData :: IO ()
introData = do
  putStrLn $ toInfo "Iniciando demonstração do TAD Data"
  putStrLn $ toInfo "Este TAD recebe como entrada uma 3-tupla de Int, que corresponde ao formato DD/MM/AAAA\n"

validData :: String -> Either String String
validData s = do
  let d = Data.fromTuple (read s :: (Int, Int, Int))
  if Data.isInvalida d then Left "Data Inválida, tente novamente" else Right s

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
  putStrLn $ toInfo "Convertendo a string num TAD Data com os seguintes argumentos -> " ++ d'' ++ " Data " ++ (show Data.vazia)
  let cData = Data.converteData d'' Data.vazia
  putStrLn $ toSuccess "Data convertida -> " ++ toBold (show cData)
  let iData   = "27/07/2021"
      sData   = Data.somaDias (Data.converteData iData Data.vazia) 5
      sData'  = Data.somaDias (Data.converteData iData Data.vazia) 43
      iData'  = "31/12/2020"
      sData'' = Data.somaDias (Data.converteData iData' Data.vazia) 10
  putStrLn $ toInfo "Executando função somaDias/2...\n"
  putStrLn (yellow (iData ++ " + 5 dias -> ") ++ toBold (Data.show sData))
  putStrLn (yellow (iData ++ " + 43 dias -> ") ++ toBold (Data.show sData'))
  putStrLn (yellow (iData' ++ " + 10 dias -> ") ++ toBold (Data.show sData''))
  putStrLn $ toSuccess "\nFim demo TAD Data!\n"    
  
-- | Clientes

introClientes :: IO ()
introClientes = do
  putStrLn $ toInfo "Iniciando demonstração do TAD Clientes"
  putStrLn $ toInfo "Este TAD lê clientes de um arquivo e manipula este arquivo nas operções de consulta, deleção e inserção\n"

getClientes :: IO String
getClientes = do
  path <- promptLine "caminho_csv> "
  return path

getCliente :: IO String
getCliente = do
  cliente <- promptLine "cliente> "
  return cliente

formatoCliente :: String
formatoCliente = "(Código :: Integer\n"
                 ++ "Endereço :: String\n"
                 ++ "Telefone :: String\n"
                 ++ "Data primeira compra :: String\n"
                 ++ "Data última compra :: String\n"
                 ++ "Valor última compra :: Decimal)\n"
                 ++ "Data -> DD/MM/AAAA\n"
                 ++ "Strings -> com aspas\n"

getIndex :: Int -> IO String
getIndex n = do
  idx <- promptLine $  "índice (0 a " ++ (show n) ++ ")> "
  return idx

tClienteFormat :: [ColSpec]
tClienteFormat = [numCol, def, def, def, def, def, numCol]

titulos :: HeaderSpec
titulos = titlesH ["Nome", " Endereço", "Tel", "Dt pri compra", "Dt ult com", "Valor ult compra"]

runClientes :: IO ()
runClientes = do
  introClientes
  putStrLn $ toInfo "Informe o caminho para o arquivo `.csv`"
  path <- getClientes
  clientesT <- Clientes.carregaClientes path
  clientes <- return $ take 5 clientesT
  putStrLn $ toInfo "Esses são os 5 primeiros clientes do arquivo:"
  ct <- return $ map (Clientes.toList) clientes
  putStrLn $ tableString tClienteFormat
                         unicodeRoundS
                         titulos
                         [rowG c | c <- ct]
  putStrLn $ toInfo "Insira um cliente:"
  putStr $ color Magenta formatoCliente
  c <- getCliente
  let c' = read c :: (Integer, String, String, String, String, String, Decimal)
      cliente = Clientes.criaCliente c'
  Clientes.salvaCliente cliente path
  putStrLn $ toInfo "Recuperando os últimos 5 clientes do arquivo..."
  cs <- Clientes.carregaClientes path
  clientes' <- return $ take 4 (reverse cs)
  ct' <- return $ map (Clientes.toList) clientes'
  putStrLn $ tableString tClienteFormat
                         unicodeRoundS
                         titulos
                         [rowG c'' | c'' <- ct']
  putStrLn $ toInfo "Consulte um Cliente do arquivo:"
  n' <- Clientes.numClientes (return clientesT)
  n <- return $ n' - 1
  idx <- getIndex n
  gCliente <- Clientes.getCliente (return clientesT) (read idx :: Int)
  putStrLn $ toInfo "Este foi o Cliente encontrado:"
  putStrLn $ tableString tClienteFormat
                         unicodeRoundS
                         titulos
                         [rowG (Clientes.toList gCliente)]
  nA <- Clientes.numClientes (return cs)
  putStrLn . yellow $ "Número atual de Clientes -> " ++ (show nA)
  putStrLn "Delete um Cliente do arquivo:"
  idx' <- getIndex n
  _ <- Clientes.excluirCliente (return clientesT) (read idx' :: Int) path
  cs' <- Clientes.carregaClientes path
  nA' <- Clientes.numClientes (return cs')
  putStrLn . yellow $ "Número atual de Clientes -> " ++ (show  nA')
  putStrLn $ toSuccess "\nFim demo TAD Clientes!\n"      
