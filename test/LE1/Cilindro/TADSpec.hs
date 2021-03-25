module LE1.Cilindro.TADSpec where

import Test.Hspec

import LE1.Cilindro.TAD

spec :: Spec
spec = do
  describe "testa o TAD Cilindro" $ do
    let raio'   = 3.2 ::Double
    let altura' = 1.2 :: Double
    let cil'    = fromTuple (raio', altura')

    let area'   = 2 * pi * raio' * (raio' + altura')
    let volume' = pi * (raio' * raio') * altura'

    describe "tenta criar um cilindro" $ do
      it "deve retornar um cilindro com os parâmetros válidos" $ do
        let cil'' = fromTuple (raio', altura')
        getRaio cil''  `shouldBe` Just raio'
        getAltura cil'' `shouldBe` Just altura'
        isVazio cil'' `shouldBe` False

    describe "retorna os campos básicos de um cilindro" $ do
      it "deve retornar apenas a altura de um cilindro" $
        getAltura cil' `shouldBe` Just altura'

      it "deve retornar apenas o raio de um cilindro" $
        getRaio cil' `shouldBe` Just raio'

      it "deve retornar Nothing se o cilindro for vazio" $ do
        getRaio vazio `shouldBe` Nothing
        getAltura vazio `shouldBe` Nothing

    describe "testa as operações de área e volume de um cilindro" $ do
      it "deve retornar a área correta" $
        calcArea cil' `shouldBe` Just area'

      it "deve retornar o volume correto" $
        calcVolume cil' `shouldBe` Just volume'

    describe "testa se um cilindro vazio e válido" $ do
      it "deve retornar um cilindro 'vazio'" $ do
        getRaio vazio `shouldBe` Nothing
        getAltura vazio `shouldBe` Nothing

      it "deve retornar True se um cilindro estiver 'vazio'" $
        isVazio vazio `shouldBe` True
