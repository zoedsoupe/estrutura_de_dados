module LE2.Stack.DelimSpec where

import           Test.Hspec

import           LE2.Stack.Delim

erroParen :: String
erroParen = "Erro: fecha parentêses não casa!"

erroCol :: String
erroCol = "Erro: fecha colchetes não casa!"

erroChaves :: String
erroChaves = "Erro: fecha chaves não casa!"

spec :: Spec
spec = do
  describe "testa o casamento de parênteses, colchetes e chaves!" $ do
    it "deve retornar a quantidade correta de erros" $ do
      let as = "{}[]()"
      let bs = ")]}"
      let cs = "([{"
      let ds = "([]{}"
      let es = "()[{}"
      let fs = "()[]{"
      let xs = "()[]{}"
      let ys = "([{}])"
      let zs = "{[()]}"

      (length $ parse as) `shouldBe` 0
      (length $ parse bs) `shouldBe` 3
      (length $ parse cs) `shouldBe` 3
      (length $ parse ds) `shouldBe` 1
      (length $ parse es) `shouldBe` 1
      (length $ parse fs) `shouldBe` 1
      (length $ parse xs) `shouldBe` 0
      (length $ parse ys) `shouldBe` 0
      (length $ parse zs) `shouldBe` 0

    it "deve retornar os erros corretos" $ do
      let as = "([{"
      let bs = "([]{}"
      let cs = "()[{}"
      let ds = "()[]{"
      let es = "({}])"
      let fs = "([}])"
      let gs = "[{}])"
      let hs = "]]{}}{"
      let is = "(])]{{"
      let js = "{}{[))"
      let xs = "("
      let ys = "["
      let zs = "{"

      parse as `shouldBe` [erroChaves, erroCol, erroParen]
      parse bs `shouldBe` [erroParen]
      parse cs `shouldBe` [erroCol]
      parse ds `shouldBe` [erroChaves]
      parse es `shouldBe` [erroCol]
      parse fs `shouldBe` [erroChaves]
      parse gs `shouldBe` [erroParen]
      parse xs `shouldBe` [erroParen]
      parse ys `shouldBe` [erroCol]
      parse zs `shouldBe` [erroChaves]
      parse hs `shouldBe` [erroChaves, erroChaves, erroCol, erroCol]
      parse is `shouldBe` [erroChaves, erroChaves, erroCol, erroCol]
      parse js `shouldBe` [erroCol, erroChaves, erroParen, erroParen]
