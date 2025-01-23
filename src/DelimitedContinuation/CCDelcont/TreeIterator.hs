-- https://wiki.haskell.org/Library/CC-delcont
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DelimitedContinuation.CCDelcont.TreeIterator (program) where

import Control.Monad.CC (MonadDelimitedCont, reset, runCCT, shift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (fromJust)
import System.Random (randomIO)
import Data.Foldable (for_)
import System.Random.Stateful (Random)

-- ------------------------------------------------------------
-- データ型の定義

-- 単純なツリー構造
data Tree a
  = Branch a (Tree a) (Tree a)
  | Leaf
  deriving (Show)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z (Branch a l r) = f a (foldr f (foldr f z r) l)

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton a = Branch a Leaf Leaf

insert :: (Ord t) => t -> Tree t -> Tree t
insert b Leaf = Branch b Leaf Leaf
insert b (Branch a l r)
  | b < a = Branch a (insert b l) r
  | otherwise = Branch a l (insert b r)

-- ------------------------------------------------------------
data Iterator m a
  = Done
  | Current a (m (Iterator m a))

begin :: (MonadDelimitedCont p s m, Foldable Tree) => Tree a -> m (Iterator m a)
begin t = reset $ \p ->
  for_
    t
    ( \a ->
        shift p (\k -> pure (Current a (k $ pure ())))
    )
    >> pure Done

current :: Iterator m a -> Maybe a
current Done = Nothing
current (Current a _) = Just a

next :: (Monad m) => Iterator m a -> m (Iterator m a)
next Done = pure Done
next (Current _ i) = i

finished :: Iterator m a -> Bool
finished Done = True
finished _ = False

randomTree :: (Num t1, MonadIO f, Random t2, Ord t2, Eq t1) => t1 -> f (Tree t2)
randomTree n = rt empty n
  where
    rt t 0 = pure t
    rt t n = do
      r <- liftIO randomIO
      rt (insert r t) (n - 1)

program :: IO ()
program = runCCT $ do
  t <- randomTree 10
  i <- begin t
  doStuff i
  where
    doStuff i
      | finished i = pure ()
      | otherwise = do
          i' <- next i
          i'' <- next i -- this is ignored
          liftIO $ print (fromJust $ current i :: Int)
          doStuff i'