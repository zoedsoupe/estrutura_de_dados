module LE2.Stack.TAD
  ( new
  , push
  , pop
  , isEmpty
  , size
  , (<>>)
  , (>-)
  , (<<>)
  , peek
  , popWhile
  ) where

data Stack a = S Integer [a]
  deriving (Read, Show)

new :: Stack a
new = S 0 []

push :: Stack a -> a -> Stack a
push (S sz xs) x = S (succ sz) (x : xs)

pop :: Stack a -> (Maybe a, Stack a)
pop st@(S _  []) = (Nothing, st)
pop (   S sz xs) = (Just (head xs), S (pred sz) (tail xs))

isEmpty :: Stack a -> Bool
isEmpty (S _ []) = True
isEmpty (S _ _ ) = False

size :: Stack a -> Integer
size (S sz _) = sz

peek :: Stack a -> Maybe a
peek (S _ []     ) = Nothing
peek (S _ (x : _)) = Just x

-- | Helpers

-- | Insere uma lista na Stack onde o
-- último elemento da lista será o topo
(<>>) :: Stack a -> [a] -> Stack a
(<>>) s         []       = s
(<>>) (S sz xs) (y : ys) = S (succ sz) (y : xs) <>> ys

-- | Mapeia uma ação monádica numa Stack
-- descartando o resultado
(>-) :: (Monad m) => (a -> m b) -> Stack a -> m ()
(>-) f (S _ xs) = mapM_ f xs

-- | Extrai os elementos
(<<>) :: Stack a -> [a]
(<<>) (S _ []) = []
(<<>) (S _ xs) = extract xs []
 where
  extract []       zs = zs
  extract (y : ys) zs = extract ys (y : zs)

popWhile :: (a -> Bool) -> Stack a -> ([a], Stack a)
popWhile p stack =
  let (top, rest) = pop stack
  in  case top of
        Nothing -> ([], rest)
        Just x  -> if p x
          then let (others, rest') = popWhile p rest in (x : others, rest')
          else ([], rest)
