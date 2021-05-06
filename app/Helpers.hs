module Helpers where

import           System.Console.Pretty          ( Color(..)
                                                , Pretty(..)
                                                , Style(..)
                                                , color
                                                , style
                                                )
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( hFlush
                                                , stdout
                                                )

evens :: [a] -> [a]
evens (x : xs) = x : odds xs
evens _        = []

odds :: [a] -> [a]
odds (_ : xs) = evens xs
odds _        = []

exit :: IO ()
exit = do
  putStrLn $ toSuccess "Até mais!!!"
  exitSuccess

toBold :: Pretty a => a -> a
toBold s = style Bold s

toSuccess :: Pretty a => a -> a
toSuccess s = color Green s

toFailure :: Pretty a => a -> a
toFailure s = color Red s

toInfo :: Pretty a => a -> a
toInfo s = color Blue s

yellow :: Pretty a => a -> a
yellow s = color Yellow s

promptLine :: String -> IO String
promptLine prompt = do
  putStr $ color Yellow prompt
  hFlush stdout
  getLine

askUntil :: String -> (String -> (Either String String)) -> IO String
askUntil prompt confirm = go
 where
  go = do
    answer  <- promptLine prompt
    answer' <- return $ confirm answer
    case answer' of
      Left  msg -> putStr msg >> go
      Right res -> pure res

-- | Getters

getDouble :: IO Double
getDouble = do
  n <- promptLine "número> "
  return $ (read n :: Double)

getInt :: IO Integer
getInt = do
  n <- promptLine "número> "
  return $ (read n :: Integer)

getString :: IO String
getString = do
  s <- promptLine "texto> "
  return s

getRes :: String -> Either String String
getRes s = case s of
  "y" -> Right s
  "n" -> Right s
  _   -> Left "Opção inválida, tente novamente\n"

getNumbers :: String -> IO [Integer]
getNumbers prompt = go []
 where
  go xs = do
    ans <- promptLine prompt
    case ans of
      "q" -> return xs
      x   -> do
        x' <- return (read x :: Integer)
        putStrLn $ toInfo "Próximo número!"
        go (x' : xs)
