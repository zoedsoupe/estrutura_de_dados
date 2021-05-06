module Helpers.Clientes
  ( runClientes
  ) where

import           Data.Decimal                   ( Decimal )
import           Helpers                        ( promptLine
                                                , toInfo
                                                , toSuccess
                                                , yellow
                                                )
import qualified LE1.Clientes.TAD              as Clientes
import           System.Console.Pretty          ( Color(Magenta)
                                                , color
                                                )
import           Text.Layout.Table              ( ColSpec
                                                , HeaderSpec
                                                , def
                                                , numCol
                                                , rowG
                                                , tableString
                                                , titlesH
                                                , unicodeRoundS
                                                )

introClientes :: IO ()
introClientes = do
  putStrLn $ toInfo "Iniciando demonstração do TAD Clientes"
  putStrLn
    $ toInfo
        "Este TAD lê clientes de um arquivo e manipula este arquivo nas operções de consulta, deleção e inserção\n"

getClientes :: IO String
getClientes = do
  path <- promptLine "caminho_csv> "
  return path

getCliente :: IO String
getCliente = do
  cliente <- promptLine "cliente> "
  return cliente

formatoCliente :: String
formatoCliente =
  "(Código :: Integer\n"
    ++ "Endereço :: String\n"
    ++ "Telefone :: String\n"
    ++ "Data primeira compra :: String\n"
    ++ "Data última compra :: String\n"
    ++ "Valor última compra :: Decimal)\n"
    ++ "Data -> DD/MM/AAAA\n"
    ++ "Strings -> com aspas\n"

getIndex :: Int -> IO String
getIndex n = do
  idx <- promptLine $ "índice (0 a " ++ (show n) ++ ")> "
  return idx

tClienteFormat :: [ColSpec]
tClienteFormat = [numCol, def, def, def, def, def, numCol]

titulos :: HeaderSpec
titulos = titlesH
  [ "Nome"
  , " Endereço"
  , "Tel"
  , "Dt pri compra"
  , "Dt ult com"
  , "Valor ult compra"
  ]

runClientes :: IO ()
runClientes = do
  introClientes
  putStrLn $ toInfo "Informe o caminho para o arquivo `.csv`"
  path      <- getClientes
  clientesT <- Clientes.carregaClientes path
  clientes  <- return $ take 5 clientesT
  putStrLn $ toInfo "Esses são os 5 primeiros clientes do arquivo:"
  ct <- return $ map (Clientes.toList) clientes
  putStrLn
    $ tableString tClienteFormat unicodeRoundS titulos [ rowG c | c <- ct ]
  putStrLn $ toInfo "Insira um cliente:"
  putStr $ color Magenta formatoCliente
  c <- getCliente
  let c' = read c :: (Integer, String, String, String, String, String, Decimal)
      cliente = Clientes.criaCliente c'
  Clientes.salvaCliente cliente path
  putStrLn $ toInfo "Recuperando os últimos 5 clientes do arquivo..."
  cs        <- Clientes.carregaClientes path
  clientes' <- return $ take 4 (reverse cs)
  ct'       <- return $ map (Clientes.toList) clientes'
  putStrLn $ tableString tClienteFormat
                         unicodeRoundS
                         titulos
                         [ rowG c'' | c'' <- ct' ]
  putStrLn $ toInfo "Consulte um Cliente do arquivo:"
  n'       <- Clientes.numClientes (return clientesT)
  n        <- return $ n' - 1
  idx      <- getIndex n
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
  _    <- Clientes.excluirCliente (return clientesT) (read idx' :: Int) path
  cs'  <- Clientes.carregaClientes path
  nA'  <- Clientes.numClientes (return cs')
  putStrLn . yellow $ "Número atual de Clientes -> " ++ (show nA')
  putStrLn $ toSuccess "\nFim demo TAD Clientes!\n"
