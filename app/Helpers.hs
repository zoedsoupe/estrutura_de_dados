module Helpers where

import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import System.Console.Pretty (Pretty(..), Color(..), Style(..), style, color)

evens :: [a] -> [a]
evens (x:xs) = x:odds xs
evens _ = []

odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _ = []

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
      answer <- promptLine prompt
      answer' <- return $ confirm answer
      case answer' of
        Left msg -> putStr msg >> go 
        Right res -> pure res
