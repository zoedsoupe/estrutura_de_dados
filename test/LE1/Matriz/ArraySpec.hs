module LE1.Matriz.ArraySpec where

import Test.Hspec
import Control.Exception (evaluate)

import LE1.Matriz.Array
import Data.Array.Unboxed (elems, (!), array)

spec :: Spec
spec = do
  describe "testa Algoritmos Matrizes Lista" $ do
    let m = 5 :: Int
    let n = 5 :: Int

    describe "testa a criação de matrizes lista" $ do
      it "deve criar uma matriz quadrada 2x2 corretamente" $ do
        let xs = [4,7,5,10]
        let a  = matriz 2 2
        elems a `shouldBe` xs
        linhas a `shouldBe` 2
        colunas a `shouldBe` 2

    describe "testa a transposição de matrizes lista" $ do
      it "deve transpor uma matriz corretamente" $ do
        let a      = matriz m n
        let ma     = linhas a
        let na     = colunas a
        let axs    = elems a
        let ta     = transpose a
        let txs    = elems ta
        let resTxs = fromList m n [10,8,8,8,10,3,3,4,8,5,7,6,7,5,3,3,10,3,6,5,5,10,6,7,5]
        linhas ta `shouldBe` ma
        colunas ta `shouldBe` na
        axs `shouldSatisfy` (/= txs) 
        txs `shouldBe` elems resTxs

    describe "testa funções extras" $ do
      it "deve retornar uma matriz absoluta a partir de outra matriz" $ do
        let a      = fromList 2 2 [-1, -2, -3, -4]
        let axs    = elems a
        let axs'   = [1, 2, 3, 4]
        let absa   = absMatriz a
        let absaxs = elems absa
        absaxs `shouldBe` axs'
        axs `shouldSatisfy` (/= absaxs)

      it "deve retornar uma matriz negada" $ do
        let a    = matriz 2 2
        let axs  = elems a
        let na   = negateMatriz a
        let naxs = elems na
        let allN = foldr (\x _ -> if x < 0 then True else False) False naxs
        allN `shouldBe` True
        axs `shouldSatisfy` (/= naxs)
      
      it "deve retornar o elemento correto dado um par de posições" $ do
        let a = matriz 2 2
        let x = 7 :: Int
        let y = 5 :: Int
        let x' = a ! (0, 1)
        let y' = a ! (1, 0)
        x `shouldBe` x'
        y `shouldBe` y'

      it "deve lançar uma exeção caso as posições sejam incorretas" $ do
        let a = matriz 2 2
        evaluate (a ! (0, -1)) `shouldThrow` anyException
        evaluate (a ! (-1, 0)) `shouldThrow` anyException        

    describe "testa a soma de matrizes lista" $ do
      it "deve retornar uma matriz vazia caso as linhas de A e B sejam diferentes" $ do
        let a  = matriz 2 2
        let b  = matriz 3 2
        let mA = linhas a
        let mB = linhas b
        mA `shouldSatisfy` (/= mB)
        somaMatriz a b `shouldBe` array ((0,0),(-1,0)) []

      it "deve retornar uma matriz vazia caso as colunas de A e B sejam diferentes" $ do
        let a  = matriz 2 2
        let b  = matriz 2 3
        let nA = colunas a
        let nB = colunas b
        nA `shouldSatisfy` (/= nB)
        somaMatriz a b `shouldBe` array ((0,0),(-1,0)) []

      it "deve retornar a soma correta de duas matrizes A e B" $ do
        let (a, b)   = (matriz 2 2, matriz 2 2)
        let (a', b') = (matriz 3 3, matriz 3 3)
        let ab       = somaMatriz a b
        let ab'      = somaMatriz a' b'
        ab `shouldBe` fromList 2 2 [8,14,10,20]
        ab' `shouldBe` fromList 3 3 [20,14,10,8,8,12,14,10,20]

    describe "testa a multiplicação de matrizes lista" $ do
      it "deve retornar uma matriz vazia caso as colunas de A sejam diferentes das linhas de B" $ do
        let a  = matriz 2 2
        let b  = matriz 3 2
        let nA = colunas a
        let mB = linhas b
        nA `shouldSatisfy` (/= mB)
        multiplicaMatriz a b `shouldBe` array ((0,0),(-1,0)) []

      it "deve retornar uma matriz vazia caso as linhas de B sejam diferentes das colunas de A" $ do
        let a  = matriz 3 3
        let b  = matriz 2 3
        let nA = colunas a
        let mB = linhas b
        nA `shouldSatisfy` (/= mB)
        multiplicaMatriz a b `shouldBe` array ((0,0),(-1,0)) []

      it "deve retornar a multiplição correta de duas matrizes A e B" $ do
        let (a, b)   = (matriz 2 3, matriz 3 4)
        let (a', b') = (matriz 2 4, matriz 4 5)
        let ab       = multiplicaMatriz a b
        let ab'      = multiplicaMatriz a' b'
        ab `shouldBe` fromList 2 4 [115,130,87,148,109,128,69,130]
        ab' `shouldBe` fromList 2 5 [130,99,154,168,128,148,114,178,192,152]
