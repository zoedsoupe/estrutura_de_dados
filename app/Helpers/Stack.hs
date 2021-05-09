module Helpers.Stack where

import           Data.Char                      ( isSpace )
import           Data.List                      ( intercalate )
import           Helpers
import qualified LE2.Stack.TAD                 as Stack

introStack :: IO ()
introStack = do
  putStrLn $ toInfo "Iniciando a demonstração do TAD Stack"
  putStrLn
    $ toInfo
        "Para essa demonstração, serão apresentados 7 testes interativos que utilizam o TAD!\n"

binarize :: Integer -> Integer
binarize n = go n Stack.new
 where
  go 0 s = read (intercalated s) :: Integer
  go x s = go (x `div` 2) $ Stack.push s (x `mod` 2)
  intercalated s = intercalate "" $ map show $ (Stack.<<>) s

isOpenParen :: Char -> Bool
isOpenParen '(' = True
isOpenParen _   = False

isCloseParen :: Char -> Bool
isCloseParen ')' = True
isCloseParen _   = False

opPriority :: Char -> Int
opPriority '*' = 2
opPriority '/' = 2
opPriority '+' = 1
opPriority '-' = 1
opPriority _   = 0

hasPriority :: Char -> Char -> Bool
hasPriority op1 op2 | opPriority op1 > opPriority op2 = True
                    | otherwise                       = False

isOperator :: Char -> Bool
isOperator '+' = True
isOperator '-' = True
isOperator '*' = True
isOperator '/' = True
isOperator _   = False

addSpace :: String -> String
addSpace [] = []
addSpace s  = (take 1 s) ++ " " ++ addSpace (drop 1 s)

removeSpace :: String -> String
removeSpace = filter $ not . isSpace

stringfy :: Char -> String
stringfy = (: [])

sToInt :: String -> Int
sToInt s = read s :: Int

parse :: String -> IO ()
parse s = go s Stack.new
 where
  go "" st | Stack.isEmpty st = putStrLn $ toSuccess "Parsing finalizado!"
           | otherwise = putStrLn $ toFailure "Erro: abre parentêses não casa!"
  go (ch : chs) st
    | isOpenParen ch = go chs $ Stack.push st ch
    | isCloseParen ch = if Stack.isEmpty st
      then putStrLn $ toFailure "Erro: fecha parentêses não casa!"
      else let (Just _, st') = Stack.pop st in go chs st'
    | otherwise = go chs st

postfixit :: String -> IO String
postfixit s = go (removeSpace s) Stack.new ""
 where
  go [] st pos =
    let (remain, _) = Stack.popWhile ignore st
    in  return . addSpace $ pos ++ remain
  go (ch : chs) st pos
    | isOperator ch = case Stack.peek st of
      Nothing -> go chs (Stack.push st ch) pos
      Just op -> if hasPriority ch op
        then go chs (Stack.push st ch) pos
        else
          let (olders, st') = Stack.popWhile (not . hasPriority ch) st
          in  go chs (Stack.push st' ch) $ pos ++ olders
    | otherwise = go chs st (pos ++ [ch])
  ignore _ = True

calc :: String -> IO String
calc s = go ss Stack.new
 where
  ss = map stringfy $ removeSpace s
  go [] st = let (Just result, _) = Stack.pop st in return result
  go (x : xs) st
    | isOperator $ head x
    = let (Just a, st'  ) = Stack.pop st
          (b     , st''') = case Stack.pop st' of
            (Just y , st'') -> (y, st'')
            (Nothing, st'') -> ("0", st'')
          calculated = case x of
            "+" -> add a b
            "-" -> sub a b
            "*" -> mult a b
            "/" -> d a b
      in  go xs $ Stack.push st''' calculated
    | otherwise
    = go xs $ Stack.push st x
  add x y = show $ sToInt x + sToInt y
  sub x y = show $ sToInt x - sToInt y
  mult x y = show $ sToInt x * sToInt y
  d x y = show $ div (sToInt x) (sToInt y)

inversao :: IO ()
inversao = do
  -- É... Eu sei... Não me julgue...
  xs <- fmap reverse $ getNumbers "número> "
  let stack = Stack.new Stack.<>> xs
  print Stack.>- stack
  putStrLn $ toInfo "\nGostaria de inverter mais séries numéricas? (y/n)"
  res <- askUntil "resposta> " getRes
  if res == "y" then inversao else putStrLn $ toSuccess "\nFim teste Stack 1!\n"

conversao :: IO ()
conversao = do
  putStrLn $ toInfo "Insira um número:"
  n    <- getInt
  nbin <- return $ binarize n
  putStrLn
    .  toSuccess
    $  "O número informado em binário é: "
    ++ show nbin
    ++ "!\n"
  putStrLn $ toInfo "Gostaria de converter mais algum número? (y/n)"
  res <- askUntil "resposta> " getRes
  if res == "y" then conversao else putStrLn $ toSuccess "Fim teste Stack 2!\n"

validacao :: IO ()
validacao = do
  putStrLn $ toInfo "Insira a cadeia de parentêses:"
  s <- getString
  parse s
  putStrLn $ toInfo "\nGostaria de validar mais alguma cadeia? (y/n)"
  res <- askUntil "resposta> " getRes
  if res == "y" then validacao else putStrLn $ toSuccess "Fim teste Stack 3!\n"

posfixa :: IO ()
posfixa = do
  putStrLn $ toInfo "Insira a expressão a ser convertida:"
  s  <- getString
  s' <- postfixit s
  putStrLn . toSuccess $ "Sua expressão em notação posfixa é: " ++ s'
  putStrLn $ toInfo "\nGostaria de converter mais alguma expressão? (y/n)"
  res <- askUntil "resposta> " getRes
  if res == "y" then posfixa else putStrLn $ toSuccess "Fim teste Stack 4!\n"

calcula :: IO ()
calcula = do
  putStrLn $ toInfo "Insira a expressão a ser calculada:"
  s         <- getString
  resultado <- calc s
  putStrLn . toSuccess $ "O resultado da sua expressão posfixa é: " ++ resultado
  putStrLn $ toInfo "\nGostaria de calcular mais alguma expressão? (y/n)"
  res <- askUntil "resposta> " getRes
  if res == "y" then calcula else putStrLn $ toSuccess "Fim teste Stack 5!\n"


runStack :: IO ()
runStack = do
  introStack
  putStrLn $ toInfo "Teste número 1 -> Inversão de dados!"
  putStrLn
    $ toInfo
        "Dado uma cadeia de números, o programa imprimirá os elementos na ordem inversa a que foram inseridas!\n"
  putStrLn $ toInfo
    "Insira um número por linha e insira \"q\" quando quiser prosseguir!"
  inversao
  putStrLn $ toInfo "Teste número 2 -> Conversão decimal para binário!"
  putStrLn
    $ toInfo
        "Dado um número decimal inteiro, o programa irá retornar o seu equivalente na base 2!\n"
  conversao
  putStrLn $ toInfo "Teste número 3 -> Validação de \"código fonte\"!"
  putStrLn
    $ toInfo
        "Insira uma cadeia de \"(\" e \")\" para que o programa identifique se todos os pares estão corretos!\n"
  validacao
  putStrLn $ toInfo "Teste número 4 -> Notação infixa para posfixa!"
  putStrLn
    $ toInfo
        "Dado uma expressão em notação infixa, o programa irá convertê-la para a notação posfixa!\n"
  posfixa
  putStrLn $ toInfo "Teste número 5 -> Avaliação de expressões posfixas!"
  putStrLn
    $ toInfo
        "Dado uma expressão em notação posfixa, o programa irá calcular seu resultado!\n"
  calcula
