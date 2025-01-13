{-
http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-j.pdf
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module DelimitedContinuation.Example2 where

import Control.Monad.CC (shift, reset, MonadDelimitedCont, runCCT)
import Control.Monad.IO.Class (MonadIO (liftIO))

{-
  通常の再帰を使った版
-}
data Tree
  = Empty
  | Node Tree Int Tree
  deriving (Eq, Show)

walk :: Tree -> IO ()
walk Empty = pure ()
walk (Node t1 n t2) = do
  walk t1
  print n
  walk t2

program :: IO ()
program = do
  let tree = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)
  walk tree

{-
  限定継続を使った版
-}
data ResultT m a
  = Done
  | Next Int (m a -> m (ResultT m a))

-- shift :: (MonadDelimitedCont p s m) => p b -> ((m a -> m b) -> m b) -> m a
-- (m a -> m b) -> m b
yield :: MonadDelimitedCont p s m => p (ResultT m a) -> Int -> m a
yield p n = shift p (\k -> pure $ Next n k)

walk2 :: MonadDelimitedCont p s m => p (ResultT m a) -> Tree -> m ()
walk2 _ Empty = pure ()  -- 空の木の場合、何もせず戻る
walk2 p (Node t1 n t2) = do
  walk2 p t1     -- 左部分木を走査
  yield p n      -- ノードの値 'n' を出力
  walk2 p t2     -- 右部分木を走査

start :: MonadDelimitedCont p s m => Tree -> m (ResultT m a)
start tree = reset \p -> do
  walk2 p tree
  pure Done

printNodes :: MonadIO m => Tree -> m ()
printNodes tree = do
  runCCT $ loop (start tree)
  where
    loop :: (MonadDelimitedCont p s m, MonadIO m) => m (ResultT m ()) -> m ()
    loop z = do
      x <- z
      case x of
        Done -> pure ()  -- もうノードがない場合、終了
        (Next n k) -> do
          liftIO $ print n     -- ノードの値 'n' を表示
          loop (k $ pure ())   -- 継続 'k' を評価し、次のノードを表示

program2 :: IO ()
program2 = do
  let tree = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)
  printNodes tree

deepTree :: Int -> Tree
deepTree 0 = Empty
deepTree n = Node (deepTree (n - 1)) n Empty