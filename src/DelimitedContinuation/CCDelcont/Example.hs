module DelimitedContinuation.CCDelcont.Example (program) where

import Control.Monad.CC (shift, reset, CC, runCC)
import Control.Monad (forM_)

{-
  CC-delcont の例
  新しいGHCだとビルドできないので、自前でビルドしたCC-delcontを使う
-}

data Iterator r a = I a (CC r (Iterator r a)) | Done

current :: Iterator r a -> Maybe a
current (I a _) = Just a
current Done    = Nothing

next :: Iterator r a -> CC r (Iterator r a)
next (I _ m) = m
next Done    = return Done

iterator :: ((a -> CC r ()) -> CC r ()) -> CC r (Iterator r a)
iterator loop = reset $ \p ->
                 loop (\a ->
                    shift p $ \k ->
                        pure $ I a (k $ return ())) >> return Done

test :: CC r [Int]
test = do i <- iterator $ forM_ [1..5]
          go [] i
 where
 go l Done = return l
 go l i    = do let (Just a) = current i
                    l' = replicate a a ++ l
                i' <- next i
                go l' i'


program :: IO ()
program = print $ runCC test
