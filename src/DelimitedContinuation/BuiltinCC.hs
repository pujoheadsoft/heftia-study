{-
  Haskell 9.6.6からは、限定継続を実現するための機能が提供されるようになった。
  これはその機能を使って実装したもの。

  ARATA Mizuki(mod_poppo)氏のブログ記事に書かれていたHaskellの実装。
  https://blog.miz-ar.info/2022/10/delimited-continuations/

  newPrompt, pushPrompt, withSubCont, pushSubContなどの関数を書いてくれていたので、
  shiftなどは自分で書けた。
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}

module DelimitedContinuation.BuiltinCC where

import GHC.Exts
import GHC.IO ( IO(IO) )
import Control.Monad.IO.Class
import Control.Monad.Identity (IdentityT)
import Control.Monad.Trans (lift)

type role CC nominal representational

newtype CC ans a = CC (State# RealWorld -> (# State# RealWorld, a #))
  deriving (Functor, Applicative, Monad) via IO

newtype CCT ans m a = CCT { unCCT :: IdentityT m a }

runCC :: (forall ans. CC ans a) -> a
runCC (CC m) = case runRW# m of (# _, a #) -> a

type role Prompt nominal representational

data Prompt ans a = Prompt (PromptTag# a)

newPrompt :: CC ans (Prompt ans a)
newPrompt = CC $ \s1 -> case newPromptTag# s1 of
  (# s2, tag #) -> (# s2, Prompt tag #)

pushPrompt :: Prompt ans a -> CC ans a -> CC ans a
pushPrompt (Prompt tag) (CC m) = CC $ prompt# tag m

type SubCont ans a b = CC ans a -> CC ans b

withSubCont :: Prompt ans b -> (SubCont ans a b -> CC ans b) -> CC ans a
withSubCont (Prompt tag) f = CC $ control0# tag $ \k ->
  case f (\(CC m) -> CC (k m)) of CC m -> m

pushSubCont :: SubCont ans a b -> CC ans a -> CC ans b
pushSubCont = id

reset :: (Prompt ans b -> CC ans b) -> CC ans b
reset e = newPrompt >>= \p -> pushPrompt p (e p)

shift :: Prompt ans a -> ((b -> CC ans a) -> CC ans a) -> CC ans b
shift p f = withSubCont p $ \sk -> pushPrompt p $ f (pushPrompt p . pushSubCont sk . pure)

shift0 :: Prompt ans a -> ((b -> CC ans a) -> CC ans a) -> CC ans b
shift0 p f = withSubCont p $ \sk -> f (pushPrompt p . pushSubCont sk . pure)

control :: Prompt ans a -> ((b -> CC ans a) -> CC ans a) -> CC ans b
control p f = withSubCont p $ \sk -> pushPrompt p $ f (pushSubCont sk . pure)

control0 :: Prompt ans a -> ((b -> CC ans a) -> CC ans a) -> CC ans b
control0 p f = withSubCont p $ \sk -> f (pushSubCont sk . pure)

abort :: Prompt ans b -> CC ans b -> CC ans a
abort p e = withSubCont p (const e)

program :: IO ()
program = do
  let computation = reset \p -> do
          (3 *) <$> shift p ( \k -> do
                  x <- k 5
                  y <- k x
                  pure $ 1 + y
              )
  print (runCC computation)