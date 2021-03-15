module LE1.Exercicio1
  ( Cilindro (..)
  , initCilindro
  , getAltura
  , getRaio
  , area
  , volume
  ) where

-- Implementação

-- Record que representa um cilindro com raio e altura!
type Altura = Double
type Raio = Double

data Cilindro = Cilindro { raio :: Raio
                         , altura :: Altura
                         } deriving (Show, Eq)

-- Interface pública

{- Recebe uma 2-Tupla ou Par, onde os 2 elemetos são Double
   O primeiro elemento dessa 2-Tupla é o raio e o segundo, a altura
   do Cilindro que será retornado -}
initCilindro :: (Double, Double) -> Cilindro
initCilindro (r, a) = Cilindro {raio = r, altura = a}

{- Dado um Cilindro, retorna apenas a altura
   Realizo uma "Correspondência de Padroões"
   me importando apenas com que o parâmetro seja
   um Cilindro e com sua altura -}
getAltura :: Cilindro -> Altura
getAltura (Cilindro _ a) = a

-- Dado um Cilindro, retorno apenas o raio
getRaio :: Cilindro -> Raio
getRaio (Cilindro r _) = r

{- Calculo a área de um Cilindro seguindo a fórmula:
   A = 2πr^2 + h(2πr)
   onde A = 2πr(r + a) -}
area :: Cilindro -> Double
area (Cilindro r a) = (*) com (r + a)
  where com = (* pi) 2 * r

{- Dado um Cilindro, calculo seu volume com a seguinte
   fórmula: V = πr2h -}
volume :: Cilindro -> Double
volume (Cilindro r a) = pi * r * ((*) 2 a)

