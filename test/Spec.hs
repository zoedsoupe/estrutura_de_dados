import Test.Hspec

import LE1.Exercicio1

main :: IO ()
main = hspec $ do
  describe "testa o TAD Cilindro" $ do
    let raio' = 3.2
    let altura' = 1.2
    let cil' = initCilindro(raio', altura')

    let area' = 2 * pi * raio' * (raio' + altura')
    let volume' = pi * raio' * (2 * altura')

    describe "tenta criar um cilindro" $ do
      it "deve retornar um cilindro com os parâmetros válidos" $ do
        initCilindro (raio', altura') `shouldBe` Cilindro {altura = altura', raio = raio'}

    describe "retorna os campos básicos de um cilindro" $ do
      it "deve retornar apenas a altura de um cilindro" $
        getAltura cil' `shouldBe` altura'

      it "deve retornar apenas o raio de um cilindro" $
        getRaio cil' `shouldBe` raio'

    describe "testa as operações de área e volume de um cilindro" $ do
      it "deve retornar a área correta" $
        area cil' `shouldBe` area'

      it "deve retornar o volume correto" $
        volume cil' `shouldBe` volume'
