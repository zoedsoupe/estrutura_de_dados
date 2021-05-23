module LE1.Recursao.RaizSpec where

import           Test.Hspec

import           LE1.Recursao.Raiz

spec :: Spec
spec = do
  describe "testa a função de raiz quadradada" $ do
    it "deve retornar -1 caso o número seja negativo" $ do
      let xs = [ nSqrt (n) 2 0.01 | n <- map (negate) [1 .. 10] ]
      all (== -1) xs `shouldBe` True

    it "deve retornar -1 caso a aproximação seja negativa" $ do
      let xs = [ nSqrt 15 (a) 0.01 | a <- map (negate) [1 .. 10] ]
      all (== -1) xs `shouldBe` True

    it "deve retornar -1 caso a tolerância seja negativa" $ do
      let xs = [ nSqrt 15 7 (i) | i <- map (negate) [1 .. 10] ]
      all (== -1) xs `shouldBe` True

    it "deve retornar -1 caso a tolerância seja 0" $ do
      let x = nSqrt 15 7 0.0
      (x == -1) `shouldBe` True

    it "deve calcular a raiz quadrada corretamente" $ do
      nSqrt 5 2 0.01 `shouldBe` 2.2360679779158037
      nSqrt 4 2 0.01 `shouldBe` 2.0
      nSqrt 3 1 0.01 `shouldBe` 1.7320508100147274
