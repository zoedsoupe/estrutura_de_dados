module LE2.Stack.DelimSpec where

import           Test.Hspec

import           LE2.Stack.Delim

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
      let ks = "([{})"
      let ls = "([{])"
      let ms = "([{}]"
      let xs = "("
      let ys = "["
      let zs = "{"

      parse xs `shouldBe` [erroAbreParen]
      parse ys `shouldBe` [erroAbreCol]
      parse zs `shouldBe` [erroAbreChaves]
      parse as `shouldBe` [erroAbreParen, erroAbreCol, erroAbreChaves]
      parse bs `shouldBe` [erroAbreParen]
      parse cs `shouldBe` [erroAbreCol]
      parse ds `shouldBe` [erroAbreChaves]
      parse es `shouldBe` [erroFechaCol]
      parse fs `shouldBe` [erroFechaChaves]
      parse gs `shouldBe` [erroFechaParen]
      parse hs `shouldBe` [erroFechaCol, erroFechaCol, erroFechaChaves, erroAbreChaves]
      parse is `shouldBe` [erroFechaCol, erroFechaCol, erroAbreChaves, erroAbreChaves]
      parse js `shouldBe` [erroAbreChaves, erroAbreCol, erroFechaParen, erroFechaParen]
      parse ks `shouldBe` [erroAbreCol]
      parse ls `shouldBe` [erroAbreChaves]
      parse ms `shouldBe` [erroAbreParen]
