module DelimitedContinuation.CCDelcont.TreeExample where

import Control.Monad.CC (MonadDelimitedCont, reset, runCCT, shift)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Foldable (for_)

-- ------------------------------------------------------------
-- データ型の定義

-- 単純なツリー構造
data Tree a = Leaf | Branch a (Tree a) (Tree a)

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton a = Branch a Leaf Leaf

insert :: (Ord t) => t -> Tree t -> Tree t
insert b Leaf = Branch b Leaf Leaf
insert b (Branch a l r)
  | b < a = Branch a (insert b l) r
  | otherwise = Branch a l (insert b r)

fold :: (a -> b -> b -> b) -> b -> Tree a -> b
fold _ z Leaf = z
fold f z (Branch a l r) = f a (fold f z l) (fold f z r)

for :: (Monad m) => Tree a -> (a -> m b) -> m ()
for t f = fold (\a l r -> l >> f a >> r) (return ()) t
-- ------------------------------------------------------------

data Iterator m a
  = Done
  | Cur a (m (Iterator m a))

begin :: (MonadDelimitedCont p s m) => Tree a -> m (Iterator m a)
begin t = reset $ \p ->
  for_
    t
    ( \a ->
        shift p (\k -> return (Cur a (k $ return ())))
    )
    >> return Done

current :: Iterator m a -> Maybe a
current Done = Nothing
current (Cur a _) = Just a

next :: (Monad m) => Iterator m a -> m (Iterator m a)
next Done = return Done
next (Cur _ i) = i

finished :: Iterator m a -> Bool
finished Done = True
finished _ = False

main :: IO ()
main = runCCT $ do
  t <- randomTree 10
  i <- begin t
  doStuff i
  where
    doStuff i
      | finished i = return ()
      | otherwise = do
          i' <- next i
          i'' <- next i -- this is ignored
          liftIO $ print (fromJust $ current i :: Int)
          doStuff i'

randomTree :: t1 -> m (Tree t2)
randomTree n = rt empty n
  where
    rt t 0 = return t
    rt t n = do
      r <- liftIO randomIO
      rt (insert r t) (n - 1)
