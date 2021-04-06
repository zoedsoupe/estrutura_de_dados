module LE1.Recursao.CombinacaoSpec where

import Test.Hspec

import LE1.Recursao.Combinacao

spec :: Spec
spec = do
  describe "testa o algoritmo de combinação" $ do
    it "deve retornar 1 se k == 0" $ do
      combina 2 0 `shouldBe` 1
      combina 10 3 `shouldBe` 120      

    it "deve retornar 1 se n == k" $ do
      combina 2 2 `shouldBe` 1
      combina 6 2 `shouldBe` 15      

    it "deve retornar 1 caso k < 0" $ do
      combina 2 (-1) `shouldBe` 1
      combina 2 1 `shouldBe` 2

    it "deve retornar 0 se k > n" $ do
      combina 2 3 `shouldBe` 0
      combina 20 3 `shouldBe` 1140

    it "combina corretamente n e k" $ do
      combina 20 4 `shouldBe` 4845
      combina 15 7 `shouldBe` 6435
      combina 49 6 `shouldBe` 13983816

    
