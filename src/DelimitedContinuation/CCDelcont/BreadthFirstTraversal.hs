module DelimitedContinuation.CCDelcont.BreadthFirstTraversal (program) where

import Control.Monad.CC (MonadDelimitedCont, control, reset, runCCT)

data Tree a
  = Node a (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

toList :: Tree a -> [a]
toList (Leaf i) = [i]
toList (Node a t1 t2) = a : toList t1 ++ toList t2

visit :: MonadDelimitedCont p s m => p [a] -> Tree a -> m ()
visit p = visit'
 where
 visit' (Leaf i)       = control p $ \k -> (i:) <$> k (pure ())
 visit' (Node i t1 t2) = control p $ \k -> do a <- k (pure ())
                                              visit' t2
                                              visit' t1
                                              pure (i : a)

bf :: MonadDelimitedCont p s m => Tree a -> m [a]
bf t = reset $ \p -> visit p t >> pure []

program :: IO ()
program = do
  let
    t :: Tree Int
    t = Node 1 (Node 2 (Leaf 3)
                   (Node 4  (Leaf 5)
                            (Leaf 6)))
           (Node 7 (Node 8  (Leaf 9)
                            (Leaf 10))
                   (Leaf 11))
  r <- runCCT $ bf t
  print r