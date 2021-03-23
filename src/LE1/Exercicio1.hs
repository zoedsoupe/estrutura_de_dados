module LE1.Exercicio1
  ( fromTuple
  , getAltura
  , getRaio
  , calcArea
  , calcVolume
  , vazio
  , isVazio
  , valido
  ) where

-- Implementação

{- Um TAD que representa um Cilindro
   que possui uma altura A e um raio R

   Também defino apelidos para os tipos
   de retorno e campos -}
type Altura = Double
type Raio = Double
type Area = Double
type Volume = Double

data Cilindro = Cilindro { raio :: Raio
                         , altura :: Altura
                         } deriving (Show, Eq, Ord)

-- Contrato das implementações

vazio :: Cilindro
isVazio :: Cilindro -> Bool
valido :: Cilindro -> Bool
fromTuple :: (Double, Double) -> Cilindro
getAltura :: Cilindro -> Altura
getRaio :: Cilindro -> Raio
calcArea :: Cilindro -> Area
calcVolume :: Cilindro -> Volume

-- Interface pública

vazio = Cilindro 0 0

isVazio (Cilindro 0 0) = True
isVazio _              = False

valido (Cilindro r a)
  | r <= 0    = False
  | a <= 0    = False
  | otherwise = True

{- Dado um Cilindro, retorna apenas a altura
   Realizo uma "Correspondência de Padroões"
   me importando apenas com que o parâmetro seja
   um Cilindro e com sua altura -}
getAltura (Cilindro _ a) = a

-- Dado um Cilindro, retorno apenas o raio
getRaio (Cilindro r _) = r

{- Calculo a área de um Cilindro seguindo a fórmula:
   A = 2πr^2 + h(2πr)
   onde A = 2πr(r + h) -}
calcArea (Cilindro r a) = (*) com (r + a)
  where com = (* pi) 2 * r

{- Dado um Cilindro, calculo seu volume com a seguinte
   fórmula: V = πr^2h -}
calcVolume (Cilindro r a) = pi * (r * r) * a

{- Recebe uma 2-Tupla ou Par, onde os 2 elemetos são Double
   O primeiro elemento dessa 2-Tupla é o raio e o segundo, a altura
   do Cilindro que será retornado -}
fromTuple (r, a) = Cilindro r a
