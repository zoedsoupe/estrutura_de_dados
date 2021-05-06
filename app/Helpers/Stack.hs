module Helpers.Stack where

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
  intercalated s = intercalate "" $ map show $ Stack.extract s

isOpenParen :: Char -> Bool
isOpenParen '(' = True
isOpenParen _   = False

isCloseParen :: Char -> Bool
isCloseParen ')' = True
isCloseParen _   = False

parse :: String -> IO ()
parse s = go s Stack.new
 where
  go "" st | Stack.isEmpty st = putStrLn $ toSuccess "Parsing finalizado!"
           | otherwise = putStrLn $ toFailure "Erro: abre parentêses não casa!"
  go (ch : chs) st
    | isOpenParen ch = go chs $ Stack.push st ch
    | isCloseParen ch = if Stack.isEmpty st
      then putStrLn $ toFailure "Erro: fecha parentêses não casa!"
      else let Just (st', _) = Stack.pop st in go chs st'
    | otherwise = go chs st

-- É... Eu sei... Não me julgue...
inversao :: IO ()
inversao = do
  xs <- fmap reverse $ getNumbers "número> "
  let stack = Stack.pushList Stack.new xs
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
  putStrLn $ toInfo "Gostaria de validar mais alguma cadeia? (y/n)"
  res <- askUntil "resposta> " getRes
  if res == "y" then validacao else putStrLn $ toSuccess "Fim teste Stack 3!\n"

runStack :: IO ()
runStack = do
  introStack
  putStrLn $ toInfo "Teste número 1 -> Inversão de dados!"
  putStrLn
    $ toInfo
        "Dado uma cadeia de números, o programa imprimirá os elementos na ordem inversa a que foram inseridas"
  putStrLn $ toInfo
    "Insira um número por linha e insira \"q\" quando quiser prosseguir!\n"
  inversao
  putStrLn $ toInfo "Teste número 2 -> Conversão decimal para binário!"
  putStrLn
    $ toInfo
        "Dado um número decimal inteiro, o programa irá retornar o seu equivalente na base 2!\n"
  conversao
  putStrLn $ toInfo "Teste número 3 -> Validação de \"código fonte\"!"
  putStrLn
    $ toInfo
        "Insira uma cadeia de \"(\" e \")\" para que o programa identifique se todos os pares estão corretos!"
  validacao
