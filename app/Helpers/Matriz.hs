{-# LANGUAGE BangPatterns #-}
module Helpers.Matriz (runMatriz) where

import LE1.Matriz.Medidores
import Control.Monad (forM_)
import qualified LE1.Matriz.Lista as L
import qualified LE1.Matriz.Array as A
import Helpers (toBold, toInfo, toSuccess, evens)

introMatriz :: IO ()
introMatriz = do
  putStrLn $ toInfo "Iniciando demonstração sobre Matrizes!"
  putStrLn $ toInfo "Implementei 2 tipos de Matriz -> com Arrays e com Linked Lists para fazer uma comparação\n"

medeLista :: IO ()
medeLista = do
  let (a100, b100)   = (L.matriz 100 100, L.matriz 100 100)     :: (L.Matriz Int, L.Matriz Int)
  let (a300, b300)   = (L.matriz 300 300, L.matriz 300 300)     :: (L.Matriz Int, L.Matriz Int)
  let (a500, b500)   = (L.matriz 500 500, L.matriz 500 500)     :: (L.Matriz Int, L.Matriz Int)
  let (a1000, b1000) = (L.matriz 1000 1000, L.matriz 1000 1000) :: (L.Matriz Int, L.Matriz Int)
  start' <- start
  putStrLn $ toInfo "somando matrizes com 100 elementos..."
  timerc start' "soma matriz lista 100"
  pure $ a100 + b100
  timerc start' "fim soma lista 100"
  putStrLn $ toInfo "somando matrizes com 300 elementos..."
  timerc start' "soma matriz lista 300"
  pure $ a300 + b300
  timerc start' "fim soma lista 300"
  putStrLn $ toInfo "somando matrizes com 500 elementos..."
  timerc start' "soma matriz lista 500"
  pure $ a500 + b500
  timerc start' "fim soma lista 500"  
  putStrLn $ toInfo "somando matrizes com 1000 elementos..."
  timerc start' "soma matriz lista 1000"
  pure $ a1000 + b1000
  timerc start' "fim soma lista 1000"
  putStrLn $ toInfo "multiplicando matrizes com 100 elementos..."
  timerc start' "mult matriz lista 100"
  pure $ a100 * a100
  timerc start' "fim mult lista 100"
  putStrLn $ toInfo "multiplicando matrizes com 300 elementos..."
  timerc start' "mult matriz lista 300"
  pure $ a300 * a300
  timerc start' "fim mult lista 300"
  putStrLn $ toInfo "multiplicando matrizes com 500 elementos..."
  timerc start' "mult matriz lista 500"
  pure $ a500 * a500
  timerc start' "fim mult lista 500"
  putStrLn $ toInfo "multiplicando matrizes com 1000 elementos..."
  timerc start' "mult matriz lista 1000"
  pure $ a1000 * a1000
  timerc start' "fim mult lista 1000"  
  end <- getVals start'
  forM_ (evens $ timert end) (putStrLn . toSuccess)

medeArray :: IO ()
medeArray = do
  let (a100, b100)   = (A.matriz 100 100, A.matriz 100 100)     :: (A.Matriz, A.Matriz)
      (a300, b300)   = (A.matriz 300 300, A.matriz 300 300)     :: (A.Matriz, A.Matriz)
      (a500, b500)   = (A.matriz 500 500, A.matriz 500 500)     :: (A.Matriz, A.Matriz)
      (a1000, b1000) = (A.matriz 1000 1000, A.matriz 1000 1000) :: (A.Matriz, A.Matriz)
  start' <- start
  putStrLn $ toInfo "somando matrizes com 100 elementos..."
  timerc start' "soma matriz lista 100"
  let !_ =  A.somaMatriz a100 b100
  timerc start' "fim soma lista 100"
  putStrLn $ toInfo "somando matrizes com 300 elementos..."  
  timerc start' "soma matriz lista 300"
  let !_ =  A.somaMatriz a300 b300
  timerc start' "fim soma lista 300"
  putStrLn $ toInfo "somando matrizes com 500 elementos..."  
  timerc start' "soma matriz lista 500"
  let !_ =  A.somaMatriz a500 b500
  timerc start' "fim soma lista 500"
  putStrLn $ toInfo "somando matrizes com 1000 elementos..."  
  timerc start' "soma matriz lista 1000"
  let !_ = A.somaMatriz a1000 b1000
  timerc start' "fim soma lista 1000"
  putStrLn $ toInfo "multiplicando matrizes com 100 elementos..."
  timerc start' "mult matriz lista 100"
  let !_ = A.multiplicaMatriz a100 b100
  timerc start' "fim mult lista 100"
  putStrLn $ toInfo "multiplicando matrizes com 300 elementos..."  
  timerc start' "mult matriz lista 300"
  let !_ = A.multiplicaMatriz a300 b300
  timerc start' "fim mult lista 300"
  putStrLn $ toInfo "multiplicando matrizes com 500 elementos..."
  timerc start' "mult matriz lista 500"
  let !_ = A.multiplicaMatriz a500 b500
  timerc start' "fim mult lista 500"
  putStrLn $ toInfo "multiplicando matrizes com 1000 elementos..."
  timerc start' "mult matriz lista 1000"
  let !_ = A.multiplicaMatriz a1000 b1000
  timerc start' "fim mult lista 100"    
  end <- getVals start'
  forM_ (evens $ timert end) (putStrLn . toSuccess)

runLista :: IO ()
runLista = do
  putStrLn $ toInfo "Iniciando demonstração Matriz Lista!\n"
  putStrLn $ toInfo "Criando Matriz exemplo A 5x5.."
  let a = L.matriz 5 5 :: (L.Matriz Int)
  L.printMatriz a
  putStrLn $ toInfo "Transposta de A:"
  L.printMatriz $ L.transpose a
  putStrLn $ toInfo "Negação de A:"
  L.printMatriz $ negate a
  putStrLn $ toInfo "Soma A + A:"
  L.printMatriz $ a + a
  putStrLn $ toInfo "Multiplicação A + A:"
  L.printMatriz $ a * a
  putStrLn $ (toBold . toInfo) "\nIniciando teste de carga Matriz Lista!"
  medeLista
  putStrLn $ toSuccess "Fim demo Matriz Lista!\n"

runArray :: IO ()
runArray = do
  putStrLn $ toInfo "Iniciando demonstração Matriz Array!\n"
  putStrLn $ toInfo "Criando Matriz exemplo A 10x10.."
  let a = A.matriz 10 10
  A.printMatriz a
  putStrLn $ toInfo "Transposta de A:"
  A.printMatriz $ A.transpose a
  putStrLn $ toInfo "Negação de A:"
  A.printMatriz $ A.negateMatriz a
  putStrLn $ toInfo "Soma A + A:"
  A.printMatriz $ A.somaMatriz a a
  putStrLn $ toInfo "Multiplicação A + A:"
  A.printMatriz $ A.multiplicaMatriz a a
  putStrLn $ (toBold . toInfo) "\nIniciando teste de carga Matriz Array!"
  medeArray
  putStrLn $ toSuccess "Fim demo Matriz Array!\n"

runMatriz :: IO ()
runMatriz = do
  introMatriz
  runLista
  runArray
  putStrLn $ toSuccess "Fim demo Matrizes!\n"
