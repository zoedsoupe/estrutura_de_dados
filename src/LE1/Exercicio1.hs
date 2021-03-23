module LE1.Exercicio1
  ( fromTuple
  , getAltura
  , getRaio
  , calcArea
  , calcVolume
  , vazio
  , isVazio
  , valido
  , toTuple
  ) where

-- | Implementação

{- | Um TAD que representa um Cilindro
     que possui uma altura A e um raio R

     Também defino apelidos para os tipos
     de retorno e campos -}
type Altura = Double
type Raio = Double
type Area = Double
type Volume = Double

data Cilindro = Vazio | Cilindro { raio :: Raio
                                 , altura :: Altura
                                 } deriving (Show, Eq, Ord)

-- | Interface Pública

{- | Devolve um TAD Cilindro vazio -}
vazio :: Cilindro
vazio = Vazio

{- | Por correspondência de valor, verifico
     se um Cilindro é Vazio -}
isVazio :: Cilindro -> Bool
isVazio Vazio = True
isVazio _     = False

{- | Um Cilindro Vazio não é válido pois
     algebraicamente não é possível existir
     um formato "Vazio" -}
valido :: Cilindro -> Bool
valido Vazio = False
valido _     = True

{- | Dado um Cilindro, retorna apenas a altura
     Realizo uma "Correspondência de Padroões"
     me importando apenas com que o parâmetro seja
     um Cilindro e com sua altura

     Aqui introduzo a Mônada "Maybe", que representa
     uma computação com "Sucesso" ou "Falha", sem lançar
     exceções!

     Caso o resultado seja o esperado, retorno
     uma instância de "Just" e o resultado, caso contrário
     retorno uma instância de "Nothing" -}
getAltura :: Cilindro -> Maybe Altura
getAltura Vazio          = Nothing
getAltura (Cilindro _ a) = Just a

{- | Dado um Cilindro, retorno apenas o raio,
     seguindo a correspondência de valores e
     o encapsulamento na Mônada "Maybe" -}
getRaio :: Cilindro -> Maybe Raio
getRaio Vazio          = Nothing
getRaio (Cilindro r _) = Just r

{- | Calculo a área de um Cilindro seguindo a fórmula:

     A = 2πr^2 + h(2πr)
     onde A = 2πr(r + h)

     Se o Cilindro for Vazio, não possui área (: -}
calcArea :: Cilindro -> Maybe Area
calcArea Vazio          = Nothing
calcArea (Cilindro r a) = Just area
  where area = (*) (r + a) . twice $ (*) pi r

{- | Dado um Cilindro, calculo seu volume com a seguinte
     fórmula: V = πr^2h

     Seguindo a lógica da área, um Cilindro Vazio
     não possui volume -}
calcVolume :: Cilindro -> Maybe Volume
calcVolume Vazio          = Nothing
calcVolume (Cilindro r a) = Just volume
  where volume = pi * (r * r) * a

{- | Recebe uma 2-Tupla ou Par, onde os 2 elemetos são Double
     O primeiro elemento dessa 2-Tupla é o raio e o segundo, a altura
     do Cilindro que será retornado -}
fromTuple :: (Double, Double) -> Cilindro
fromTuple (r, a) = Cilindro r a

{- | Dado um Cilindro válido, devolvo uma 2-Tupla, ou Par -}
toTuple :: Cilindro -> Maybe (Double, Double)
toTuple Vazio          = Nothing
toTuple (Cilindro r a) = Just (r, a)

-- | Funções de ajuda
twice :: Floating a => a -> a
twice x = 2 * x
