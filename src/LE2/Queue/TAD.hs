module LE2.Queue.TAD
  ( new
  , isEmpty
  , enq
  , deq
  , size
  , front
  , rear
  ) where

data Queue a = Q Integer [a] Integer [a]
  deriving (Read, Show)

instance Semigroup (Queue a) where
  (<>) _ _ = undefined

instance Monoid (Queue a) where
  mempty = Q 0 [] 0 []

new :: Queue a
new = mempty

isEmpty :: Queue a -> Bool
isEmpty (Q _ [] _ []) = True
isEmpty _             = False

enq :: Queue a -> a -> Queue a
enq (Q x xs y ys) w = Q x xs (succ y) (w : ys)

deq :: Queue a -> (Maybe a, Queue a)
deq q@(Q _ [] _ []) = (Nothing, q)
deq (  Q x [] y ys) = deq $ Q y (reverse ys) x []
deq (  Q x xs y ys) = (Just (head xs), Q (pred x) (tail xs) y ys)

size :: Queue a -> Integer
size (Q x _ y _) = x + y

front :: Queue a -> Maybe a
front (Q _ [] _ []) = Nothing
front (Q _ [] _ ys) = Just (last ys)
front (Q _ xs _ _ ) = Just (head xs)

rear :: Queue a -> Maybe a
rear (Q _ [] _ []) = Nothing
rear (Q _ xs _ []) = Just (head xs)
rear (Q _ _  _ ys) = Just (head ys)
