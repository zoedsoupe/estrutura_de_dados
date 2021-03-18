module LE1.Exercicio2Spec where

import Test.Hspec

import LE1.Exercicio2

spec :: Spec
spec = do
  describe "testa o TAD Conjunto de Inteiros" $ do
    let vazio' = []
    let xs     = [1,2,3,4]

    it "deve criar um conjunto vazio" $
      criaConjunto `shouldBe` vazio'

    it "deve inserir um elemento ao onjunto" $
      (elem 5) (insereItem 5 xs) `shouldBe` True

    it "deve remover um elemento do conjunto" $
      (elem 4) (removeItem 4 xs) `shouldBe` False

    describe "testa se um elemento pertence ou não a um conjunto" $ do
      it "deve retorna True se pertence" $
        pertence 4 xs `shouldBe` True

      it "deve retornar False se não pertence" $
        pertence 5 xs `shouldBe` False

    it "deve retornar o menor valor de um conjunto" $
      minEl xs `shouldBe` 1

    describe "testa a união de dois conjuntos" $ do
      let ys    = [4,5,6,1]
      let unido = [1,2,3,4,5,6]

      it "deve retornar o sejunto conjunto caso o primeiro seja vazio" $
        uniao vazio' ys `shouldBe` ys

      it "deve unir corretamente os dois conjuntos" $
        uniao xs ys `shouldBe` unido

    describe "testa se um conjunto é igual a outro" $ do
      let ys = [1,2,3,4]
      let zs = [4,5,6]

      it "deve retornar True se os conjuntos são iguais" $
        igual xs ys `shouldBe` True

      it "deve retornar False se os conjuntos diferem-se" $
        igual xs zs `shouldBe `False

    describe "testa se um conjunto é vazio" $ do
      it "deve retornar True se um conjunto é vazio" $
        isVazio vazio' `shouldBe` True

      it "deve retornar False se o conjunto possui elementos" $
        isVazio xs `shouldBe` False
