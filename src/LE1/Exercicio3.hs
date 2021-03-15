module LE1.Exercicio3
  ( Data (..)
  , imprimeData
  , converteData
  , somaDias
  , dataPadrao
  ) where

import Text.Printf (printf)

-- Implementação

data Data = Data { dia :: Int
                 , mes :: Int
                 , ano :: Int
                 } deriving (Show, Eq, Ord)

-- Interface

{- Dado uma 3-Tupla (tripla) com dia, mes e ano,
   imprimo na tela uma string com a data formatada

   Caso a data seja inválida, retorno Nothing -}
imprimeData :: (Int, Int, Int) -> Maybe String
imprimeData (d, m ,a) = (case data' of
                           Nothing -> Nothing
                           Just Data {} -> Just dataValida)
                             where data'      = criaData (d, m, a)
                                   dataValida =  (printf "%d/%d/%d" d m a) :: String

{- Dado uma string que possua uma data válida,
   retorno o TAD data correspondente

   Em qualquer outra possibilidde, retorno Nothing -}
converteData :: String -> Data -> Maybe Data
converteData d Data {} = (case criaData (d', m, a) of
                            Nothing -> Nothing
                            Just Data {} -> Just Data {dia = d', mes = m, ano = a})
                              where d':m:a:_ = map (\x -> read x :: Int) (separa (=='/') d)

{- Somo os dias de forma recursiva

   O caso base é quando os dias a serem somados são 0

   Se o dia e mes da data forem 1 e os dias a serem
   somados forem maior que o tamnho do ano da data,
   aumento o ano e chamo novamente a função com os dias
   a serem somados menos o tamnho do ano da data

   Para qualquer outra data, faço a seguinte verificação:

   Sendo x os dias a serem somados, temos que

   1 - se x é negativo? -> Nothing
   2 - se x >= os dias restantes do ano -> ano + 1 e
       x = x - tamanho do ano
   3 - se x >= os dias restantes no mes ->
       se o mes == 12, aumento o ano, senão aumento o mes e
       x = x - dias restantes do mes
   4 - caso genérico -> retorno uma data com dias somados a x -}
somaDias :: Data -> Int -> Maybe Data
somaDias data' 0 = Just data'
somaDias (Data 1 1 y) d'
  | d' < 0         = Nothing
  | d' >= tamAno y = somaDias (Data 1 1 (y + 1)) (d' - tamAno y)
somaDias data'@(Data d m y) d'
  | d' < 0                 = Nothing
  | d' >= faltaEmAno data' = somaDias (Data 1 1 (y + 1)) (d' - faltaEmAno data')
  | d' >= faltaEmMes data' = somaDias (if m == 12 then Data 1 1 (y + 1) else Data 1 (m + 1) y) (d' - faltaEmMes data')
  | otherwise              = Just (Data (d + d') m y)

-- Funções de ajuda

dataPadrao :: Data
dataPadrao = Data {dia = 27, mes = 7, ano = 2001}

-- Cria uma "validação" de uma data
criaData :: (Int, Int, Int) -> Maybe Data
criaData (d, m, a)
  | d < 1 || d > 31      = Nothing
  | m < 1 || m > 12      = Nothing
  | a < 1920 || a > 2021 = Nothing
  | m == 2 && d > 29   = Nothing
  | otherwise            = Just Data {dia = d, mes = m, ano = a}

anoBissexto :: Int -> Bool
anoBissexto n = (mod) n 4 == 0 && ((mod) n 100 /= 0 || (mod) n 400 == 0)

tamAno :: Int -> Int
tamAno n = if anoBissexto n then 366 else 365

-- De forma "imperativa", pego quantos dias tem um mês
tamMes :: Int -> Int -> Int
tamMes a' m' = meses !! (m' - 1) where
  meses   = if anoBissexto a' then meses'' else meses'
  meses'  = [31,28,31,30,31,30,31,31,30,31,30,31]
  meses'' = [31,29,31,30,31,30,31,31,30,31,30,31]

faltaEmMes :: Data -> Int
faltaEmMes (Data d m y) = tamMes y m - d + 1

{- Calcula há quantos dias o ano começou,
   fazendo a soma de todos os dias dos meses
   passados + os dias do mes atual - 1 -}
diasInicioAno :: Data -> Int
diasInicioAno (Data d m y) = mesesAnterioriores + d - 1 where
 mesesAnterioriores = sum [tamMes y m' | m' <- deleta m [1..m]]

faltaEmAno :: Data -> Int
faltaEmAno data' = tamAno (ano data') - inicio
  where inicio = diasInicioAno data'

{- Dado uma função como (=='-') e uma String,
   retorno uma lista de String separada por '-' -}
separa :: (Char -> Bool) -> String -> [String]
separa p s = case dropWhile p s of
              "" -> []
              s' -> w : separa p s''
                where (w, s'') = break p s'  

{- Dado uma lista de elementos,
   deleta o elemento "deleted"
   e retorna a nova lista -}
deleta :: Eq a => a -> [a] -> [a]
deleta deleted xs = [ x | x <- xs, x /= deleted ]
