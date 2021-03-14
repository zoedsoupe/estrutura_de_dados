import Test.Hspec

import LE1.Exercicio1
import LE1.Exercicio2

main :: IO ()
main = hspec $ do
  describe "testa o TAD Cilindro" $ do
    let raio'   = 3.2
    let altura' = 1.2
    let cil'    = initCilindro(raio', altura')

    let area'   = 2 * pi * raio' * (raio' + altura')
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
      LE1.Exercicio2.min xs `shouldBe` 1

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
        vazio vazio' `shouldBe` True

      it "deve retornar False se o conjunto possui elementos" $
        vazio xs `shouldBe` False
