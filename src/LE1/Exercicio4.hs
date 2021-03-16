module LE1.Exercicio4
  ( Cliente (..)
  , Clientes
  , criaCliente
  , getCliente
  , carregaClientes
  , salvaCliente
  , salvaClientes
  , excluirCliente
  , numClientes
  , clientePadrao
  ) where

import Data.List (isInfixOf)
import Data.Decimal (Decimal)
import System.PosixCompat.Files (getFileStatus, isDirectory)
import qualified Data.ByteString.Lazy.Char8 as L

-- Implementação

data Cliente = Cliente { codigo :: Integer
                       , nome :: L.ByteString
                       , endereco :: L.ByteString
                       , telefone :: L.ByteString
                       , dt_primeira_compra :: L.ByteString
                       , dt_ultima_compra :: L.ByteString
                       , valor_ultima_compra :: Decimal
                       } deriving (Show, Eq)

type Clientes = [Cliente]

-- Interface

{- Dado os seguintes parâmetros, em ordem:
   código, nome, endereço, telefone,
   data primeira compra, data última compra
   e valor da última compra, retorno um Cliente -}
criaCliente :: (Integer, String, String, String, String, String, Decimal) -> Cliente
criaCliente (c, n, e, t, dt_p, dt_u, v_u) = Cliente c n' e' t' dt_p' dt_u' v_u
  where n'    = L.pack n
        e'    = L.pack e
        t'    = L.pack t
        dt_p' = L.pack dt_p
        dt_u' = L.pack dt_u

clientePadrao :: Cliente
clientePadrao = Cliente c n e t dt_p dt_u v_u
  where c     = 423 :: Integer
        n     = L.pack "Joao"
        e     = L.pack "Av. Alberto"
        t     = L.pack "(22)12345-6789"
        dt_p  = L.pack "27/07/2001"
        dt_u  = L.pack "27/07/2012"
        v_u   = 123.67 :: Decimal

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
getCliente :: IO Clientes -> Int -> IO Cliente
getCliente c_io idx = do
  num <- numClientes c_io
  c   <- c_io
  if idx > num || idx < 0
    then return $ c !! (mod (negate idx) num)
    else return $ c !! idx

{- Dado um caminho de um arquivo,
   leio o conteúdo desse arquivo
   e devolvo uma lista de Clientes -}
carregaClientes :: FilePath -> IO Clientes
carregaClientes path = do
  conteudo <- leArquivo path
  case conteudo of
    [[]] -> return []
    _ -> do 
      clientes <- return $ map (leCliente) conteudo
      return clientes

{- Dado um Cliente e um caminho, adiciono esse
   Cliente no arquivo, acrescentando caso o
   arquivo já exista -}
salvaCliente :: Cliente -> FilePath -> IO Cliente
salvaCliente c path = do
  conteudo  <- return $ converteCliente c
  conteudo' <- return . L.pack $ show conteudo
  _         <- L.appendFile path conteudo'
  return c

{- Dada uma lista de CLientes, envolvida por uma
   Mônada IO, salvo um Cliente por vex, um por linha -}
salvaClientes :: IO Clientes -> FilePath -> IO Clientes 
salvaClientes xs path = do
  xs' <- xs
  _   <- return [salvaCliente x path | x <- xs']
  xs

{- Dada uma IO lista de Clientes, um índice e um caminho,
   removo dessa lista o Cliente do índice específicado
   (levando em conta a função getCliente), e crio um novo
   arquivo com a nova lista!

   Para ter o efeito de atualizar um arquivo já existe,
   forneça como parâmetro um arquivo já existe, pois essa
   função irá sobrescrevê-lo -}
excluirCliente :: IO Clientes -> Int -> FilePath -> IO Cliente
excluirCliente cs idx path = do
  cs'      <- cs
  cl       <- getCliente cs idx
  cl'      <- return cl
  cs''     <- return $ filter (/= cl') cs'
  conteudo <- return . L.unlines $ map (converteCliente) cs''
  _        <- L.writeFile path conteudo
  return cl

-- Funções de ajuda

numClientes :: IO Clientes -> IO Int
numClientes xs = return . length =<< xs

{- Converto um TAD Cliente para uma representação binária -}
converteCliente :: Cliente -> L.ByteString
converteCliente (Cliente c n t e dt_p dt_u v_u) = cliente'
  where cod      = L.pack $ show c
        va       = L.pack $ show v_u
        cliente  = cod:n:t:e:dt_p:dt_u:va:[]
        cliente' = L.intercalate (L.pack ",") cliente

{- Transformo uma lista de ByTeString (dados crus do Cliente)
   em um TAD Cliente válido -}
leCliente :: [L.ByteString] -> Cliente
leCliente xs = Cliente cod n e t dt_p dt_u va_u
  where (c:n:t:e:dt_p:dt_u:v_u:_) = xs
        cod  = read (L.unpack c) :: Integer
        va_u = read (L.unpack v_u) :: Decimal


{- Dado um caminho para um arquivo, leio o conteúdo
   dele, separo por linhas e depois divido cada linha
   em um elemento a partir do caractere ",", retornando
   uma 2d-lista de ByteString -}
leArquivo :: FilePath -> IO [[L.ByteString]]
leArquivo caminho = do
  status <- getFileStatus caminho
  if isDirectory status
    then return [[]]
    else if not $ isInfixOf ".csv" caminho
         then return [[]]
         else do
    conteudo <- L.readFile caminho
    return [L.split ',' l | l <- L.lines conteudo]
