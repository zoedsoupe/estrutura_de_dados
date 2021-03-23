module LE1.Exercicio3
  ( imprimeData
  , converteData
  , somaDias
  , toTuple
  , isVazia
  , vazia
  , isInvalida
  , fromTuple
  , mes
  , ano
  , dia
  , show
  ) where

import Data.List (intercalate)
import Text.Printf (printf)

-- Implementação

type Dia = Int
type Mes = Int
type Ano = Int

{- | Um TAD Data no qual possui 3 contrutores:

     1 - Invalida
     2 - Vazia
     3 - Uma Data com dia, mes e ano -}
data Data = Invalida | Vazia |  Data { dia :: Dia
                                     , mes :: Mes
                                     , ano :: Ano
                                     } deriving (Eq, Ord)

-- | Instancia para imprimir na tela de forma customizada
instance Show Data where
  show (Data d m a) = intercalate "/" (map (show) [d, m, a])
  show Invalida     = "Data Inválida"
  show Vazia        = "Data Vazia"

-- Interface

vazia :: Data
vazia = Vazia

isInvalida :: Data -> Bool
isInvalida Invalida = True
isInvalida _        = False

isVazia :: Data -> Bool
isVazia Vazia = True
isVazia _     = False

toTuple :: Data -> Maybe (Int, Int, Int)
toTuple (Data d m a) = Just (d, m, a)
toTuple _            = Nothing

fromTuple :: (Int, Int, Int) -> Data
fromTuple d = criaData d

{- | Dado uma 3-Tupla (tripla) com dia, mes e ano,
     retorno uma string envolvida por uma Mônada IO,
     que significa que essa string tem efeitos colaterais

     No caso, de ser impressa na tela

     Caso a data seja inválida, ou vazia, retorno
     a represdentação em string desse construtor -}
imprimeData :: (Int, Int, Int) -> IO String
imprimeData (d, m ,a) = (case data' of
                           Invalida -> return "Invalida"
                           Vazia    -> return "Estrutura vazia"
                           Data {}  -> return dataValida)
                             where data'      = criaData (d, m, a)
                                   dataValida =  (printf "%d/%d/%d" d m a) :: String

{- | Dado uma string que possua uma data válida,
     retorno o TAD Data correspondente -}
converteData :: String -> Data -> Data
converteData [] d           = d
converteData _ (Data _ _ _) = Invalida
converteData _ Invalida     = Invalida
converteData d Vazia        = (case criaData (d', m, a) of
                                 Invalida -> Invalida
                                 Vazia    -> Vazia
                                 Data {}  -> Data {dia = d', mes = m, ano = a})
                                   where d':m:a:_ = map (\x -> read x :: Int) (separa (=='/') d)

{- | Somo os dias de forma recursiva

     O caso base é quando os dias a serem somados são 0

     Se o dia e mes da data forem 1 e os dias a serem
     somados forem maior que o tamanho do ano da data,
     aumento o ano e chamo novamente a função com os dias
     a serem somados menos o tamnho do ano da data

     Para qualquer outra data, faço a seguinte verificação:

     Sendo x os dias a serem somados, temos que

     1 - se x é negativo? -> Data Invalida
     2 - se x >= os dias restantes do ano -> ano + 1 e
         x = x - tamanho do ano
     3 - se x >= os dias restantes no mes ->
           se o mes == 12, aumento o ano, senão aumento o mes e
           x = x - dias restantes do mes
     4 - caso genérico -> retorno uma data com dias somados a x -}
somaDias :: Data -> Int -> Data
somaDias data' 0 = data'
somaDias (Data 1 1 y) d'
  | d' < 0         = Invalida
  | d' >= tamAno y = somaDias (Data 1 1 (y + 1)) (d' - tamAno y)
somaDias data'@(Data d m y) d'
  | d' < 0                 = Invalida
  | d' >= faltaEmAno data' = somaDias (Data 1 1 (y + 1)) (d' - faltaEmAno data')
  | d' >= faltaEmMes data' = somaDias (if m == 12 then Data 1 1 (y + 1) else Data 1 (m + 1) y) (d' - faltaEmMes data')
  | otherwise              = Data (d + d') m y
somaDias Invalida _ = Invalida
somaDias Vazia d    = somaDias (Data 1 1 2021) d

-- Funções de ajuda

-- | Cria uma "validação" de uma data
criaData :: (Int, Int, Int) -> Data
criaData (d, m, a)
  | d < 1 || d > 31      = Invalida
  | m < 1 || m > 12      = Invalida
  | a < 1920 || a > 2021 = Invalida
  | m == 2 && d > 29     = Invalida
  | otherwise            = Data {dia = d, mes = m, ano = a}

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
faltaEmMes Invalida     = -1
faltaEmMes Vazia        = 0
faltaEmMes (Data d m y) = tamMes y m - d + 1

{- | Calcula há quantos dias o ano começou,
     fazendo a soma de todos os dias dos meses
     passados + os dias do mes atual - 1 -}
diasInicioAno :: Data -> Int
diasInicioAno Invalida     = -1
diasInicioAno Vazia        = 0
diasInicioAno (Data d m y) = mesesAnterioriores + d - 1 where
 mesesAnterioriores = sum [tamMes y m' | m' <- deleta m [1..m]]

faltaEmAno :: Data -> Int
faltaEmAno data' = tamAno (ano data') - inicio
  where inicio = diasInicioAno data'

{- | Dado uma função como (=='-') e uma String,
     retorno uma lista de String separada por '-' -}
separa :: (Char -> Bool) -> String -> [String]
separa p s = case dropWhile p s of
              "" -> []
              s' -> w : separa p s''
                where (w, s'') = break p s'  

{- | Dado uma lista de elementos,
     deleta o elemento "deleted"
     e retorna a nova lista -}
deleta :: Eq a => a -> [a] -> [a]
deleta deleted xs = [ x | x <- xs, x /= deleted ]
