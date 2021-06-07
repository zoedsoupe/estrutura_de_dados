module LE2.Listas.TAD
  ( new
  , pop
  , push
  ) where

import           Data.IORef
import           Prelude                 hiding ( last )

data Node a = Node
  { prev  :: IORef (Maybe (Node a))
  , value :: a
  , next  :: IORef (Maybe (Node a))
  }

data List a = List
  { first :: IORef (Maybe (Node a))
  , last  :: IORef (Maybe (Node a))
  }

new :: IO (List a)
new = List <$> newIORef Nothing <*> newIORef Nothing

push :: List a -> a -> IO ()
push list value = do
  last' <- readIORef $ last list
  case last' of
    Nothing -> do
      node <- Node <$> newIORef Nothing <*> pure value <*> newIORef Nothing
      writeIORef (first list) (Just node)
      writeIORef (last list)  (Just node)
    Just last' -> do
      node <- Node <$> newIORef (Just last') <*> pure value <*> newIORef Nothing
      writeIORef (next last') (Just node)
      writeIORef (last list)  (Just node)

pop :: List a -> IO (Maybe a)
pop list = do
  last' <- readIORef (last list)
  case last' of
    Nothing    -> pure Nothing
    Just last' -> do
      prev' <- readIORef $ prev last'
      case prev' of
        Nothing -> do
          writeIORef (first list) Nothing
          writeIORef (last list)  Nothing
        Just prev' -> do
          writeIORef (next prev') Nothing
          writeIORef (last list)  (Just prev')
      pure $ Just $ value last'
