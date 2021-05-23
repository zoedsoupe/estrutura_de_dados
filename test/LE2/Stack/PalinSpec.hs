module LE2.Stack.PalinSpec where

import           Test.Hspec

import           LE2.Stack.Palin

spec :: Spec
spec = do
  describe "testa a função isPalin" $ do
    let frase1 = "A base do teto desaba."
    let frase2 = "A diva em Argel alegra-me a vida."
    let frase3 = "Adias a data da saída."
    let frase4 = "Socorram-me, subi no ônibus em Marrocos."
    let frase5 = "A paz mundial não é somente possível, mas inevitável."
    let
      frase6
        = "Os espelhos permitem-nos ver objectos que não conseguimos ver directamente."
    let frase7 = "Só é preciso uma pequena quantidade."
    let frase8
          = "As pessoas devem olhar para mim para a consistência e perseverança."
    let frase9  = "Olé! Maracujá, caju, caramelo!"
    let frase10 = "Você ainda será vulnerável à vigilância direcionada."

    it "deve verificar corretamente se uma frase é palíndroma" $ do
      isPalin frase1 `shouldBe` True
      isPalin frase2 `shouldBe` True
      isPalin frase3 `shouldBe` True
      isPalin frase4 `shouldBe` True
      isPalin frase5 `shouldBe` False
      isPalin frase6 `shouldBe` False
      isPalin frase7 `shouldBe` False
      isPalin frase8 `shouldBe` False
      isPalin frase9 `shouldBe` True
      isPalin frase10 `shouldBe` False
