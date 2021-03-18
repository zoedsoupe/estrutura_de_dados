module LE1.Exercicio1Spec where

import Test.Hspec

import LE1.Exercicio1

spec :: Spec
spec = do
  describe "testa o TAD Cilindro" $ do
    let raio'   = 3.2 ::Double
    let altura' = 1.2 :: Double
    let cil'    = fromTuple (raio', altura')

    let area'   = 2 * pi * raio' * (raio' + altura')
    let volume' = pi * raio' * (2 * altura')

    describe "tenta criar um cilindro" $ do
      it "deve retornar um cilindro com os parâmetros válidos" $ do
        let cil'' = fromTuple (raio', altura')
        getRaio cil''  `shouldBe` raio'
        getAltura cil'' `shouldBe` altura'
        isVazio cil'' `shouldBe` False

    describe "retorna os campos básicos de um cilindro" $ do
      it "deve retornar apenas a altura de um cilindro" $
        getAltura cil' `shouldBe` altura'

      it "deve retornar apenas o raio de um cilindro" $
        getRaio cil' `shouldBe` raio'

    describe "testa as operações de área e volume de um cilindro" $ do
      it "deve retornar a área correta" $
        calcArea cil' `shouldBe` area'

      it "deve retornar o volume correto" $
        calcVolume cil' `shouldBe` volume'

    describe "testa se um cilindro vazio e válido" $ do
      it "deve retornar um cilindro 'vazio'" $ do
        getRaio vazio `shouldBe` 0
        getAltura vazio `shouldBe` 0

      it "deve retornar True se um cilindro é válido" $ do
        valido vazio `shouldBe` False
        valido cil' `shouldBe` True

      it "deve retornar True se um cilindro estiver 'vazio'" $
        isVazio vazio `shouldBe` True
