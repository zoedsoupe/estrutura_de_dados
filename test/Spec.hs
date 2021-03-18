import Test.Hspec

import LE1.Exercicio2
import LE1.Exercicio3
import LE1.Exercicio4

import System.IO (readFile)
import System.Directory (removeFile)
import System.IO.Temp (writeSystemTempFile)
import Data.Decimal (Decimal)

criaClientes :: Int -> Clientes -> Clientes
criaClientes x xs
  | x <= 0    = xs
  | otherwise = criaClientes (x - 1) (clientePadrao:xs)

main :: IO ()
main = hspec $ do
  describe "testa TAD Clientes" $ do
    let clientesPeq    = "./src/LE1/clientes_small.csv"
    let numClientesPeq = 30 :: Int
    let numClientesMed = 340 :: Int
    let clientesMed    = "./src/LE1/clientes_medium.csv"
    let cod            = 423 :: Integer
    let n              = "Joao"
    let en             = "Av. Alberto"
    let tel            = "(22)12345-6789"
    let data_primeira  = "27/07/2001"
    let data_ultima    = "27/07/2012"
    let valor          = 123.67 :: Decimal
    let cliente        = clientePadrao

    describe "testa a criação de um TAD Cliente" $ do      
      it "deve criar um cliente quando os argumentos são válidos" $
        criaCliente (cod, n, en, tel, data_primeira, data_ultima, valor) `shouldBe` cliente

    describe "testa o carregamento de TADs Clientes a paritr de um arquivo" $ do
      it "deve retornar uma lista de IO clientes se o arquivo existir e for válido" $ do
        clPeq    <- carregaClientes clientesPeq
        numClPeq <- numClientes $ return clPeq
        clMed    <- carregaClientes clientesMed
        numClMed <- numClientes $ return clMed
        numClPeq `shouldBe` numClientesPeq
        numClMed `shouldBe` numClientesMed

      it "deve lançar uma exeção caso o arquivo não exista ou seja inválido" $ do
        carregaClientes "./package.yaml" `shouldReturn` []
        carregaClientes "./app" `shouldReturn` []

    describe "testa o salvamento de apenas um Cliente" $ do
      it "deve salvar corretamente um cliente e atualizar um arquivo existente" $ do
        conteudo <- readFile clientesPeq
        tmp_path <- writeSystemTempFile "clientes_tmp.csv" conteudo
        _ <- salvaCliente cliente tmp_path
        numClientes (carregaClientes tmp_path) `shouldReturn` 31
        removeFile tmp_path

      it "deve salvar corretamente um cliente e criar um novo arquivo" $ do
        let tmp_path = "./clientes_tmp.csv"
        _ <- salvaCliente cliente tmp_path
        numClientes (carregaClientes tmp_path) `shouldReturn` 1
        removeFile tmp_path

    describe "testa o salvamento de multiplos clientes" $ do
      let clientes = return $ criaClientes 12 []

      it "deve salvar corretamente vários clientes e atualizar um arquivo existente" $ do
        conteudo <- readFile clientesPeq
        tmp_path <- writeSystemTempFile "clientes_tmp.csv" conteudo
        _ <- salvaClientes clientes tmp_path
        numClientes (carregaClientes tmp_path) `shouldReturn` 42
        removeFile tmp_path

      it "deve salvar corretamente vários clientes e criar um novo arquivo" $ do
        let tmp_path = "./clientes_tmp.csv"
        _ <- salvaClientes clientes tmp_path
        numClientes (carregaClientes tmp_path) `shouldReturn` 12
        removeFile tmp_path      
      
