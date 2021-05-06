module LE1.Cilindro.TAD
  ( fromTuple
  , getAltura
  , getRaio
  , calcArea
  , calcVolume
  , vazio
  , isVazio
  , toTuple
  ) where

type Altura = Double
type Raio = Double
type Area = Double
type Volume = Double

data Cilindro = Vazio | Cilindro { raio :: Raio
                                 , altura :: Altura
                                 } deriving (Show, Eq)

vazio :: Cilindro
vazio = Vazio

isVazio :: Cilindro -> Bool
isVazio Vazio = True
isVazio _     = False

getAltura :: Cilindro -> Maybe Altura
getAltura Vazio          = Nothing
getAltura (Cilindro _ a) = Just a

getRaio :: Cilindro -> Maybe Raio
getRaio Vazio          = Nothing
getRaio (Cilindro r _) = Just r

calcArea :: Cilindro -> Maybe Area
calcArea Vazio          = Nothing
calcArea (Cilindro r a) = Just area
  where area = (*) (r + a) . twice $ (*) pi r

calcVolume :: Cilindro -> Maybe Volume
calcVolume Vazio          = Nothing
calcVolume (Cilindro r a) = Just volume where volume = pi * (r * r) * a

fromTuple :: (Double, Double) -> Cilindro
fromTuple (r, a) = Cilindro r a

toTuple :: Cilindro -> Maybe (Double, Double)
toTuple Vazio          = Nothing
toTuple (Cilindro r a) = Just (r, a)

-- | Funções de ajuda
twice :: Floating a => a -> a
twice x = 2 * x
