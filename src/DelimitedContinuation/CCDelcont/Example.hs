{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module DelimitedContinuation.CCDelcont.Example (program) where

import Control.Monad.CC (shift, reset, CC, runCC)
import Control.Monad (forM_)

{-
  CC-delcont の例
  元のソース CC.hs に書いてあったやつ
  新しいGHCだとビルドできないので、自前でビルドしたCC-delcontを使う
-}

data Iterator r a
  = Done
  | Current a (CC r (Iterator r a))

current :: Iterator r a -> Maybe a
current Done    = Nothing
current (Current a _) = Just a

next :: Iterator r a -> CC r (Iterator r a)
next (Current _ m) = m
next Done    = pure Done

iterator :: ((a -> CC r ()) -> CC r ()) -> CC r (Iterator r a)
iterator loop = reset $ \p ->
                 loop (\a ->
                    shift p $ \k ->
                        pure $ Current a (k $ pure ())) >> pure Done

test :: CC r [Int]
test = do i <- iterator $ forM_ [1..5]
          go [] i
 where
 go l Done = pure l
 go l i    = do let (Just a) = current i
                    l' = replicate a a ++ l
                i' <- next i
                go l' i'


program :: IO ()
program = print $ runCC test
