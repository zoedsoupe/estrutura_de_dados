module LE1.Clientes.TADSpec where

import Test.Hspec

import System.Directory (removeFile)
import System.IO.Temp (writeSystemTempFile)
import Data.Decimal (Decimal)

import LE1.Clientes.TAD

spec :: Spec
spec = do
  describe "testa TAD Clientes" $ do
    let clientesPeq    = "./src/LE1/Clientes/clientes_small.csv"
    let numClientesPeq = 30 :: Int
    let numClientesMed = 340 :: Int
    let clientesMed    = "./src/LE1/Clientes/clientes_medium.csv"
    let cod            = 423 :: Integer
    let n              = "Joao"
    let en             = "Av. Alberto"
    let tel            = "(22)12345-6789"
    let data_primeira  = "27/07/2001"
    let data_ultima    = "27/07/2012"
    let valor          = 123.67 :: Decimal
    let cliente        = head clientes

    describe "testa a criação de um TAD Cliente" $ do      
      it "deve criar um cliente quando os argumentos são válidos" $
        let cl = criaCliente (cod, n, en, tel, data_primeira, data_ultima, valor)
            in isInvalido cl `shouldBe` False

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
      it "deve salvar corretamente vários clientes e atualizar um arquivo existente" $ do
        conteudo <- readFile clientesPeq
        tmp_path <- writeSystemTempFile "clientes_tmp.csv" conteudo
        _ <- salvaClientes clientes tmp_path
        numClientes (carregaClientes tmp_path) `shouldReturn` 33
        removeFile tmp_path

      it "deve salvar corretamente vários clientes e criar um novo arquivo" $ do
        let tmp_path = "./clientes_tmp.csv"
        _ <- salvaClientes clientes tmp_path
        numClientes (carregaClientes tmp_path) `shouldReturn` 3
        removeFile tmp_path      

    describe "testa a consulta de um Cliente" $ do
      let clienteIO = getCliente (carregaClientes clientesPeq) 2

      it "deve retornar o mesmo cliente caso o indice seja o mesmo" $ do
        clienteTest <- getCliente (carregaClientes clientesPeq) 2
        cliente'    <- clienteIO
        clienteTest `shouldBe` cliente'

      it "deve retornar um cliente válido caso o índice seja negativo" $ do
        clienteTest <- getCliente (carregaClientes clientesPeq) (-2)
        cliente'    <- clienteIO
        clienteTest `shouldBe` cliente'

      it "deve retornar o cliente no indíce que seja o resto da divisão do tamanho da lista" $ do
        clienteTest <- getCliente (carregaClientes clientesPeq) 31
        cliente'    <- getCliente (carregaClientes clientesPeq) 29
        clienteTest `shouldBe` cliente'

      describe "testa a exclusão de um cliente" $ do
        it "deve excluir corretamente um cliente e atualizar o arquivo" $ do
          cliente' <- clienteIO
          conteudo <- readFile clientesPeq
          tmp_path <- writeSystemTempFile "clientes_tmp.csv" conteudo
          _ <- excluirCliente (carregaClientes tmp_path) 2 tmp_path
          clienteTest <- getCliente (carregaClientes tmp_path) 2
          numClientes (carregaClientes tmp_path) `shouldReturn` 29
          clienteTest `shouldSatisfy` (/= cliente')
          removeFile tmp_path

        it "deve excluir corretamente um cliente e criar um novo arquivo" $ do
          let tmp_path = "./clientes_tmp.csv"
          cliente' <- clienteIO
          _ <- excluirCliente (carregaClientes clientesPeq) 4 tmp_path
          clienteTest <- getCliente (carregaClientes clientesPeq) 4
          numClientes (carregaClientes tmp_path) `shouldReturn` 29
          clienteTest `shouldSatisfy` (/= cliente')
          removeFile tmp_path
