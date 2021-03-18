module LE1.Exercicio3Spec where

import Test.Hspec

import LE1.Exercicio3

spec :: Spec
spec = do
  describe "testa o TAD Data" $ do
    describe "testa a impressão de datas" $ do
      it "deve retornar True se a data for inválida" $
        imprimeData (42, 13, 9999) `shouldReturn` "Invalida"

      it "deve retornar uma data formatada" $
        imprimeData (27, 7, 2001) `shouldReturn` "27/7/2001"

    describe "testa a conversão de string para data" $ do
      it "deve retornar a data correta a parit de uma string" $ do
        converteData "27/7/2001" (vazia) `shouldBe` fromTuple (27, 7, 2001)
        converteData "12/12/2001" (vazia) `shouldBe` fromTuple (12, 12, 2001)
        
      it "deve retornar Nothing se fevereiro for inválido" $
        let d = isInvalida $ converteData "31/2/2000" (vazia)
            in d `shouldBe` True

      it "deve retornar Nothing se qualquer parêmetro for inválido" $ do
        let d'    = isInvalida $ converteData "0/2/2000" (vazia)
        let d''   = isInvalida $ converteData "1/0/2021" (vazia)
        let d'''  = isInvalida $ converteData "12/4/999" (vazia)
        let d'''' = isInvalida $ converteData "0/0/999" (vazia)
        d'  `shouldBe` True
        d''  `shouldBe` True
        d''' `shouldBe` True
        d'''' `shouldBe` True

    describe "testa a soma de dias" $ do
      let data' = fromTuple (27, 7, 2001)
      
      it "deve retornar a mesma data caso o número de dias seja 0" $
        somaDias data' 0 `shouldBe` data'

      it "deve retornar uma data válida caso o argumento seja vazio" $
        let d = toTuple $ somaDias (vazia) 12
            in d `shouldBe` (12, 1, 2021)

      it "deve retornar uma data inválida se os dias forem negativos" $ do
        let i  = somaDias data' (-2)
        let i'  = somaDias data' (-10)
        let i'' = somaDias data' (-90)
        isInvalida i `shouldBe` True
        isInvalida i' `shouldBe` True
        isInvalida i'' `shouldBe` True

      it "deve retornar uma data com um ano a mais" $
        let (_, _, ano') = toTuple $ somaDias data' 365
            in ano' `shouldSatisfy` (> 2001)

      it "deve retornar uma data com um mês a mais" $ do
        let s  = somaDias data' 5
        let s' = somaDias data' 10
        mes s `shouldBe` 8
        mes s' `shouldBe` 8

      it "deve retornar uma data com 2 meses a mais" $ do
        let s = somaDias data' 37
            in mes s `shouldBe` 9

      it "deve retornar uma data com virada de ano" $
        let data''  = fromTuple (31, 12, 2021) 
            in somaDias data'' 2 `shouldBe` fromTuple (2, 1, 2022)

      it "deve retornar uma data após N dias" $ do
        let s = somaDias data' 2
            in toTuple s `shouldBe` (29, 7, 2001)

