{-# LANGUAGE RankNTypes #-}
module DelimitedContinuation.Example2 where

import Control.Monad.CC (shift, Prompt, reset, CC, runCC)
import Control.Monad (forM_)

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


data ResultT a = DoneX
               | Next Int (a -> ResultT a)

-- yield :: Prompt r (ResultT ()) -> Int -> CC r (ResultT ())
-- yield p n = shift p (\k -> pure $ Next n k)


-- walk2 :: Prompt r (ResultT ()) -> Tree -> CC r ()
-- walk2 p Empty = return ()  -- 空の木の場合、何もせず戻る
-- walk2 p (Node t1 n t2) = do
--   walk2 p t1       -- 左部分木を走査
--   yield p n       -- ノードの値 'n' を出力
--   walk2 p t2       -- 右部分木を走査

-- start :: Tree -> CC r ()
-- start tree = reset \p -> do
--   walk2 p tree
--   --pure DoneX

-- printNodes :: Tree -> IO ()
-- printNodes tree = loop (start tree)
--   where
--     loop :: ResultT p -> IO ()
--     loop Done = pure ()  -- もうノードがない場合、終了
--     loop (Next n k) = do
--       print n       -- ノードの値 'n' を表示
--       loop (k ())   -- 継続 'k' を評価し、次のノードを表示