module LE2.Stack.HexaSpec where

import           Test.Hspec

import           Data.Char                      ( intToDigit
                                                , toUpper
                                                )
import           LE2.Stack.Hexa
import           Numeric                        ( showHex )

convert :: Int -> Char
convert = toUpper . intToDigit

spec :: Spec
spec = do
  describe "testa função hexarize" $ do
    let dezHex       = [convert 10]
    let onzeHex      = [convert 11]
    let dozeHex      = [convert 12]
    let trezeHex     = [convert 13]
    let catorzeHex   = [convert 14]
    let quinzeHex    = [convert 15]
    let dezesseisHex = showHex 16 ""
    let cemHex       = showHex 100 ""
    let xHex         = showHex 345 ""
    let yHex         = showHex 567 ""

    it "deve converter números > 10 e < 16 em hexademimal corretamente" $ do
      hexarize 10 `shouldBe` dezHex
      hexarize 11 `shouldBe` onzeHex
      hexarize 12 `shouldBe` dozeHex
      hexarize 13 `shouldBe` trezeHex
      hexarize 14 `shouldBe` catorzeHex
      hexarize 15 `shouldBe` quinzeHex

    it "deve converter qualquer número em hexadecimal corretamente" $ do
      hexarize 100 `shouldBe` cemHex
      hexarize 345 `shouldBe` xHex
      hexarize 567 `shouldBe` yHex
      hexarize 16 `shouldBe` dezesseisHex
