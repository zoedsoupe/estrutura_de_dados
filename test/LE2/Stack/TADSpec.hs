module LE2.Stack.TADSpec where

import           Test.Hspec

import           LE2.Stack.TAD

spec :: Spec
spec = do
  describe "testa o TAD Stack" $ do
    it "deve criar uma stack vazia corretamente" $ isEmpty new `shouldBe` True

    it "deve inserir elementos numa stack corretamente" $ do
      let s  = new
      let s' = push s (2 :: Integer)
      size s' `shouldBe` 1

      let x   = new
      let x'  = push x (10 :: Integer)
      let x'' = push x' 15
      size x'' `shouldBe` 2

    it "deve remover elementos de uma stack corretamente" $ do
      let s              = new
      let s'             = push s (1 :: Integer)
      let s''            = push s' 90
      let (Just x, s''') = pop s''
      x `shouldBe` 90
      size s''' `shouldBe` 1

    it "deve verificar se uma stack é vazia corretamente" $ do
      let s   = new
      let s2  = new
      let s2' = push s2 "a"
      isEmpty s `shouldBe` True
      isEmpty s2' `shouldBe` False

    it "deve calcular o tamanho de uma stack corretamente" $ do
      let s   = new
      let s2  = new
      let s2' = push s2 "b"
      size s `shouldBe` 0
      size s2' `shouldBe` 1
