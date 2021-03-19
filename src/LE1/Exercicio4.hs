module LE1.Exercicio4
  ( criaCliente
  , getCliente
  , carregaClientes
  , salvaCliente
  , salvaClientes
  , excluirCliente
  , numClientes
  , vazio
  , isVazio
  , isInvalido
  , clientes
  ) where

import Data.List (isInfixOf)
import Data.Decimal (Decimal)
import System.PosixCompat.Files (getFileStatus, isDirectory)
import qualified Data.ByteString.Char8 as B

-- | Contrato de implementação
data Cliente = Invalido | Vazio | Cliente { codigo :: Integer
                                          , nome :: B.ByteString
                                          , endereco :: B.ByteString
                                          , telefone :: B.ByteString
                                          , dt_primeira_compra :: B.ByteString
                                          , dt_ultima_compra :: B.ByteString
                                          , valor_ultima_compra :: Decimal
                                          } deriving (Show, Eq)

type Clientes = [Cliente]

-- | Interface Pública
  
vazio :: Cliente
isVazio :: Cliente -> Bool
isInvalido :: Cliente -> Bool
carregaClientes :: FilePath -> IO Clientes
getCliente :: IO Clientes -> Int -> IO Cliente
salvaCliente :: Cliente -> FilePath -> IO ()
salvaClientes :: Clientes -> FilePath -> IO ()
excluirCliente :: IO Clientes -> Int -> FilePath -> IO Cliente
criaCliente :: (Integer, String, String, String, String, String, Decimal) -> Cliente

-- | Implementação

vazio = Vazio

isVazio Vazio = True
isVazio _     = False

isInvalido Invalido = True
isInvalido _        = False

{- Dado os seguintes parâmetros, em ordem:
   código, nome, endereço, telefone,
   data primeira compra, data última compra
   e valor da última compra, retorno um Cliente -}
criaCliente (c, n, e, t, dt_p, dt_u, v_u) = Cliente c n' e' t' dt_p' dt_u' v_u
  where n'    = B.pack n
        e'    = B.pack e
        t'    = B.pack t
        dt_p' = B.pack dt_p
        dt_u' = B.pack dt_u

{- Dado uma lista de Clientes (resultado de carregClientes)
   Devolvo apenas 1 cliente na dada posição.

   O operador (!!) em Haskell não é seguro, portanto,
   para minimizar seu efeito, realizo uma simples conta
   onde se o índice dado for negativo, converto o índice
   para ser acessível no lista, tendo:

   Tendo índice == x,
   Se x < 0 -> troco o sinal e retorno o cliente na
      posição (-x)
   Se x > tamnho lista -> retorno o Cliente na
      posição do resto do índice pelo tamanho da lista -}
getCliente c_io idx = do
  num <- numClientes c_io
  c   <- c_io
  if idx > num || idx < 0
    then return $ c !! (mod (negate idx) num)
    else return $ c !! idx

{- Dado um caminho de um arquivo,
   leio o conteúdo desse arquivo
   e devolvo uma lista de Clientes -}
carregaClientes path = do
  conteudo <- leArquivo path
  case conteudo of
    [[]] -> return []
    _ -> do 
      clientes' <- return $ map (leCliente) conteudo
      return clientes'

{- Dado um Cliente e um caminho, adiciono esse
   Cliente no arquivo, acrescentando caso o
   arquivo já exista -}
salvaCliente c path = do
  nl       <- return $ B.pack "\n"
  conteudo <- return $ B.concat (nl:(converteCliente c):[])
  B.appendFile path conteudo

{- Dada uma lista de CBientes, envolvida por uma
   Mônada IO, salvo um Cliente por vex, um por linha -}
salvaClientes [] _    = putStrLn "Lista vazia"
salvaClientes xs path = do
  _ <- return $ mapM (\x -> salvaCliente x path) xs
  putStrLn "Os Clientes foram salvos!"

{- Dada uma IO lista de Clientes, um índice e um caminho,
   removo dessa lista o Cliente do índice específicado
   (levando em conta a função getCliente), e crio um novo
   arquivo com a nova lista!

   Para ter o efeito de atualizar um arquivo já existe,
   forneça como parâmetro um arquivo já existe, pois essa
   função irá sobrescrevê-lo -}
excluirCliente cs idx path = do
  cs'      <- cs
  cl       <- getCliente cs idx
  cl'      <- return cl
  cs''     <- return $ filter (/= cl') cs'
  conteudo <- return . B.unlines $ map (converteCliente) cs''
  _        <- B.writeFile path conteudo
  return cl

-- Funções de ajuda (funções privadas)

numClientes :: IO Clientes -> IO Int
numClientes xs = return . length . filter (/= Invalido) =<< xs

{- Converto um TAD Cliente para uma representação binária -}
converteCliente :: Cliente -> B.ByteString
converteCliente Invalido = B.pack ""
converteCliente Vazio    = B.intercalate (B.pack ",") cliente
  where (c:n:t:e:dt_p:dt_u:v_u:_) = packBist $ map (\_ -> "") ['a' .. 'g']
        cliente = c:n:t:e:dt_p:dt_u:v_u:[]
converteCliente (Cliente c n t e dt_p dt_u v_u) = cliente'
  where cod      = B.pack $ show c
        va       = B.pack $ show v_u
        cliente  = cod:n:t:e:dt_p:dt_u:va:[]
        cliente' = B.intercalate (B.pack ",") cliente

{- Transformo uma lista de ByTeString (dados crus do Cliente)
   em um TAD Cliente válido -}
leCliente :: [B.ByteString] -> Cliente
leCliente (x:_)
  | x == B.empty = Invalido
leCliente (c:n:t:e:dt_p:dt_u:v_u:_) = Cliente cod n e t dt_p dt_u va_u
  where cod  = read (B.unpack c) :: Integer
        va_u = read (B.unpack v_u) :: Decimal
leCliente _ = Invalido


{- Dado um caminho para um arquivo, leio o conteúdo
   dele, separo por linhas e depois divido cada linha
   em um elemento a partir do caractere ",", retornando
   uma 2d-lista de ByteString -}
leArquivo :: FilePath -> IO [[B.ByteString]]
leArquivo caminho = do
  status <- getFileStatus caminho
  if isDirectory status
    then return [[]]
    else if not $ isInfixOf ".csv" caminho
         then return [[]]
         else do
    conteudo <- B.readFile caminho
    return [B.split ',' l | l <- B.lines conteudo]

packBist :: [String] -> [B.ByteString]
packBist xs = map (B.pack) xs

-- | Função para Mock
clientes :: Clientes
clientes = [ Cliente { codigo = 385812323496637
                     , nome = B.pack "Holie"
                     , endereco = B.pack "Bivngstone"
                     , telefone = B.pack "52674363958"
                     , dt_primeira_compra = B.pack "28/6/2020"
                     , dt_ultima_compra = B.pack "9/11/2016"
                     , valor_ultima_compra = 25.13 :: Decimal
                     }
           , Cliente {codigo = 34812323496638
                     , nome = B.pack "ax"
                     , endereco = B.pack "Funafti"
                     , telefone = B.pack "69224640681"
                     , dt_primeira_compra = B.pack "2/8/2014"
                     , dt_ultima_compra = B.pack "11/10/2013"
                     , valor_ultima_compra = 859.14 :: Decimal
                     }
           , Cliente {codigo = 35812323496639
                     , nome = B.pack "Ealeen"
                     , endereco = B.pack "Soia"
                     , telefone = B.pack "86433170873"
                     , dt_primeira_compra = B.pack "13/11/2014"
                     , dt_ultima_compra = B.pack "7/4/2004"
                     , valor_ultima_compra = 489.2 :: Decimal
                     }
           ]
