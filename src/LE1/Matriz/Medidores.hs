module LE1.Matriz.Medidores where

import Data.Time as T
import Data.IORef

start :: IO (IORef [a])
start = newIORef []

getVals :: IORef a -> IO a
getVals = readIORef

timert :: [(String, T.UTCTime)] -> [String]
timert (_:[]) = error "1???"
timert ([]) = error "2???"
timert ((s,x):b@(s',y):z) = ((pure $ mconcat [s, " -> ", s', ": ", show (T.diffUTCTime y x)]) ++) $ case z of
                           [] -> []
                           zz -> timert (b : zz)


timera :: String -> IO [(String, T.UTCTime)] 
timera s = fmap (pure . (,) s) T.getCurrentTime

timerb :: String -> [(String, T.UTCTime)] -> IO [(String, T.UTCTime)] 
timerb s xs = (xs ++) <$> timera s

timerc :: IORef [(String, T.UTCTime)] -> String -> IO ()
timerc vr s = do
  vvv <- readIORef vr
  vvv' <- timerb s vvv
  writeIORef vr vvv'
