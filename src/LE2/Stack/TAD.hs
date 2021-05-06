module LE2.Stack.TAD
  ( new
  , push
  , pop
  , isEmpty
  , size
  , pushList
  , (>-)
  ) where

data Stack a = S Integer [a]
  deriving (Read, Show)

instance Semigroup (Stack a) where
  (<>) (S sz1 xs) (S sz2 ys) = S (sz1 + sz2) (xs <> ys)

instance Monoid (Stack a) where
  mempty = S 0 []

new :: Stack a
new = mempty

push :: Stack a -> a -> Stack a
push (S sz xs) x = S (succ sz) (x : xs)

pop :: Stack a -> Maybe (Stack a, a)
pop (S _  []) = Nothing
pop (S sz xs) = Just (S (pred sz) (tail xs), head xs)

isEmpty :: Stack a -> Bool
isEmpty (S _ []) = True
isEmpty (S _ _ ) = False

size :: Stack a -> Integer
size (S sz _) = sz

-- | Helper

pushList :: Stack a -> [a] -> Stack a
pushList s         []       = s
pushList (S sz xs) (y : ys) = pushList (S (succ sz) (y : xs)) ys

(>-) :: (Monad m) => (a -> m b) -> Stack a -> m ()
(>-) f (S _ xs) = mapM_ f xs
