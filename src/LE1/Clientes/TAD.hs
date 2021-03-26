module LE1.Clientes.TAD
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
  , toList
  ) where

import Data.List (isInfixOf)
import Data.Decimal (Decimal)
import System.PosixCompat.Files (getFileStatus, isDirectory)
import qualified Data.ByteString.Char8 as B

-- | Implementação

data Cliente = Invalido | Vazio | Cliente { codigo :: Integer
                                          , nome :: B.ByteString
                                          , endereco :: B.ByteString
                                          , telefone :: B.ByteString
                                          , dt_primeira_compra :: B.ByteString
                                          , dt_ultima_compra :: B.ByteString
                                          , valor_ultima_compra :: Decimal
                                          } deriving (Show, Eq)

type Clientes = [Cliente]

-- | Implementação

vazio :: Cliente
vazio = Vazio

isVazio :: Cliente -> Bool
isVazio Vazio = True
isVazio _     = False

isInvalido :: Cliente -> Bool
isInvalido Invalido = True
isInvalido _        = False

toList :: Cliente -> [String]
toList Vazio                           = ["Cliente Vazio"]
toList Invalido                        = ["Cliente Inválido"]
toList (Cliente c n e t dt_p dt_u v_u) = list
  where c'     = show c
        n'     = B.unpack n
        e'     = B.unpack e
        t'     = B.unpack t
        dt_p'  = B.unpack dt_p
        dt_u'  = B.unpack dt_u
        v_u'   = show v_u
        list = [c', n', e', t', dt_p', dt_u', v_u']
        

criaCliente :: (Integer, String, String, String, String, String, Decimal) -> Cliente
criaCliente (c, n, e, t, dt_p, dt_u, v_u) = Cliente c n' e' t' dt_p' dt_u' v_u
  where n'    = B.pack n
        e'    = B.pack e
        t'    = B.pack t
        dt_p' = B.pack dt_p
        dt_u' = B.pack dt_u

getCliente :: IO Clientes -> Int -> IO Cliente
getCliente c_io idx = do
  num <- numClientes c_io
  c   <- c_io
  if idx > num || idx < 0
    then return $ c !! (mod (negate idx) num)
    else return $ c !! idx

carregaClientes :: FilePath -> IO Clientes
carregaClientes path = do
  conteudo <- leArquivo path
  case conteudo of
    [[]] -> return []
    _ -> do 
      clientes' <- return $ filter (/= Invalido) (map (leCliente) conteudo)
      return clientes'

salvaCliente :: Cliente -> FilePath -> IO ()
salvaCliente c path = do
  nl       <- return $ B.pack "\n"
  conteudo <- return $ B.concat (nl:(converteCliente c):[])
  B.appendFile path conteudo

{- | Dada uma lista de Clientes salvo um
     Cliente por vez, um por linha -}
salvaClientes :: Clientes -> FilePath -> IO ()
salvaClientes [] _    = putStrLn "Lista vazia"
salvaClientes xs path = mapM_ (\x -> salvaCliente x path) xs >> putStrLn "Os Clientes foram salvos!"

excluirCliente :: IO Clientes -> Int -> FilePath -> IO Cliente
excluirCliente cs idx path = do
  cs'      <- cs
  cl       <- getCliente cs idx
  cl'      <- return cl
  cs''     <- return $ filter (/= cl') cs'
  conteudo <- return . B.unlines $ map (converteCliente) cs''
  _        <- B.writeFile path conteudo
  return cl

-- Funções de ajuda

numClientes :: IO Clientes -> IO Int
numClientes xs = return . length . filter (/= Invalido) =<< xs

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

leCliente :: [B.ByteString] -> Cliente
leCliente (x:_)
  | x == B.empty = Invalido
leCliente (c:n:t:e:dt_p:dt_u:v_u:_) = Cliente cod n e t dt_p dt_u va_u
  where cod  = read (B.unpack c) :: Integer
        va_u = read (B.unpack v_u) :: Decimal
leCliente _ = Invalido


{- |Dado um caminho para um arquivo, leio o conteúdo
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
