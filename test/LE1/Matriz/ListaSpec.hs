module LE1.Matriz.ListaSpec where

import Test.Hspec
import Control.Exception (evaluate)

import LE1.Matriz.Lista

spec :: Spec
spec = do
  describe "testa Algoritmos Matrizes Lista" $ do
    let m       = 5 :: Int
    let n       = 5 :: Int

    describe "testa a criação de matrizes lista" $ do
      it "deve criar uma matriz quadrada 2x2 corretamente" $ do
        let xs = [[4,7],[5,10]]
        let a  = matriz 2 2 :: (Matriz Int)
        valores a `shouldBe` xs
        linhas a `shouldBe` 2
        colunas a `shouldBe` 2

    describe "testa a transposição de matrizes lista" $ do
      it "deve transpor uma matriz corretamente" $ do
        let a      = matriz m n
        let ma     = linhas a
        let na     = colunas a
        let axs    = valores a
        let ta     = transpose a
        let txs    = valores ta
        let resTxs = M m n [[10,8,8,8,10],[3,3,4,8,5],[7,6,7,5,3],[3,10,3,6,5],[5,10,6,7,5]] :: (Matriz Int)
        linhas ta `shouldBe` ma
        colunas ta `shouldBe` na
        axs `shouldSatisfy` (/= txs) 
        txs `shouldBe` valores resTxs

    describe "testa funções extras" $ do
      it "deve retornar uma matriz vazia de signum caso a matriz não seja quadrada" $ do
        let a  = matriz 2 3 :: (Matriz Int)
        let xs = valores $ signum a
        xs `shouldBe` []
        
      it "deve retiornar uma matriz identidade" $ do
        let a       = matriz 3 3 :: (Matriz Int)
        let xs      = valores $ a
        let ys      = valores $ signum a
        let numOne  = 3 :: Integer
        let calcOne = foldr (\x acc -> if x == 1 then acc + 1 else acc) 0 $ concat ys :: Integer
        xs `shouldSatisfy` (/= ys)
        calcOne `shouldBe` numOne

      it "deve retornar o elemento correto dado um par de posições" $ do
        let a = matriz 2 2 :: (Matriz Int)
        let x = 7 :: Int
        let y = 5 :: Int
        let x' = a ! (1, 2)
        let y' = a ! (2, 1)
        x `shouldBe` x'
        y `shouldBe` y'

      it "deve lançar uma exeção caso as posições sejam incorretas" $ do
        let a = matriz 2 2 :: (Matriz Int)
        evaluate (a ! (0, 0)) `shouldThrow` anyException

    describe "testa a soma de matrizes lista" $ do
      it "deve retornar uma matriz vazia caso as linhas de A e B sejam diferentes" $ do
        let a  = matriz 2 2 :: (Matriz Int)
        let b  = matriz 3 2 :: (Matriz Int)
        let mA = linhas a
        let mB = linhas b
        mA `shouldSatisfy` (/= mB)
        a + b `shouldBe` M 0 0 []

      it "deve retornar uma matriz vazia caso as colunas de A e B sejam diferentes" $ do
        let a  = matriz 2 2 :: (Matriz Int)
        let b  = matriz 2 3 :: (Matriz Int)
        let nA = colunas a
        let nB = colunas b
        nA `shouldSatisfy` (/= nB)
        a + b `shouldBe` M 0 0 []

      it "deve retornar a soma correta de duas matrizes A e B" $ do
        let (a, b)   = (matriz 2 2, matriz 2 2) :: (Matriz Int, Matriz Int)
        let (a', b') = (matriz 3 3, matriz 3 3) :: (Matriz Int, Matriz Int)
        let ab       = a + b
        let ab'      = a' + b'
        ab `shouldBe` M 2 2 [[8,14],[10,20]]
        ab' `shouldBe` M 3 3 [[20,14,10],[8,8,12],[14,10,20]]

    describe "testa a multiplicação de matrizes lista" $ do
      it "deve retornar uma matriz vazia caso as colunas de A sejam diferentes das linhas de B" $ do
        let a  = matriz 2 2 :: (Matriz Int)
        let b  = matriz 3 2 :: (Matriz Int)
        let nA = colunas a
        let mB = linhas b
        nA `shouldSatisfy` (/= mB)
        a * b `shouldBe` M 0 0 []

      it "deve retornar uma matriz vazia caso as linhas de B sejam diferentes das colunas de A" $ do
        let a  = matriz 3 3 :: (Matriz Int)
        let b  = matriz 2 3 :: (Matriz Int)
        let nA = colunas a
        let mB = linhas b
        nA `shouldSatisfy` (/= mB)
        a * b `shouldBe` M 0 0 []

      it "deve retornar a multiplição correta de duas matrizes A e B" $ do
        let (a, b)   = (matriz 2 3, matriz 3 4) :: (Matriz Int, Matriz Int)
        let (a', b') = (matriz 2 4, matriz 4 5) :: (Matriz Int, Matriz Int)
        let ab       = a * b
        let ab'      = a' * b'
        ab `shouldBe` M 2 4 [[95,103,102],[44,48,48],[78,87,90]]
        ab' `shouldBe` M 2 5 [[64,60,58,96],[60,57,51,87],[64,60,58,96],[72,66,72,114]]     
