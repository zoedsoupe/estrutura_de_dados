module LE1.ConjuntoInt.TADSpec where

import           Test.Hspec

import           LE1.ConjuntoInt.TAD

spec :: Spec
spec = do
  describe "testa o TAD Conjunto de Inteiros" $ do
    let vazio' = criaConjunto
    let xs     = fromList [1, 2, 3, 4]

    it "deve criar um conjunto vazio" $ isVazio criaConjunto `shouldBe` True

    it "deve inserir um elemento ao onjunto"
      $          (contem (fromList [5])) (insereItem 5 xs)
      `shouldBe` True

    it "deve remover um elemento do conjunto"
      $          (contem (fromList [4])) (removeItem 4 xs)
      `shouldBe` False

    describe "testa se um elemento pertence ou não a um conjunto" $ do
      it "deve retorna True se pertence" $ pertence 4 xs `shouldBe` True

      it "deve retornar False se não pertence" $ pertence 5 xs `shouldBe` False

    it "deve retornar o menor valor de um conjunto" $ minEl xs `shouldBe` 1

    describe "testa a união de dois conjuntos" $ do
      let ys    = fromList [4, 5, 6, 1]
      let unido = fromList [1, 2, 3, 4, 5, 6]

      it "deve retornar o sejunto conjunto caso o primeiro seja vazio"
        $          uniao vazio' ys
        `shouldBe` ys

      it "deve unir corretamente os dois conjuntos"
        $          uniao xs ys
        `shouldBe` unido

    describe "testa se um conjunto é igual a outro" $ do
      let ys = fromList [1, 2, 3, 4]
      let ws = fromList [4, 3, 2, 1]
      let zs = fromList [4, 5, 6]

      it "deve retornar True se os conjuntos são iguais" $ do
        igual xs ws `shouldBe` True
        igual xs ys `shouldBe` True

      it "deve retornar False se os conjuntos diferem-se"
        $          igual xs zs
        `shouldBe` False

    describe "testa se um conjunto é vazio" $ do
      it "deve retornar True se um conjunto é vazio"
        $          isVazio vazio'
        `shouldBe` True

      it "deve retornar False se o conjunto possui elementos"
        $          isVazio xs
        `shouldBe` False

    describe "testa se um conjunto contem outro" $ do
      it "deve retornar True se um conjunto contem o outro" $ do
        let ws = fromList [5, 6, 7, 8]
        let zs = fromList [6, 7]

        contem zs ws `shouldBe` True
