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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DelimitedContinuation.BuiltinCC where

import GHC.Exts
import GHC.IO ( IO(IO) )
import Control.Monad.IO.Class
import Control.Monad.Trans (lift, MonadTrans)

type role CC nominal representational

newtype CC ans a = CC (State# RealWorld -> (# State# RealWorld, a #))
  deriving (Functor, Applicative, Monad, MonadIO) via IO

newtype CCT ans m a = CCT { unCCT :: m (CC ans a) }
  deriving (Functor)

instance Monad m => Applicative (CCT ans m) where
  pure x = CCT $ pure (pure x)
  (CCT mf) <*> (CCT mx) = CCT $ do
    f <- mf
    x <- mx
    pure (f <*> x)

instance Monad m => Monad (CCT ans m) where
  return = pure
  (CCT mx) >>= k = CCT $ do
    x <- mx
    pure $ x >>= \a -> let CCT my = k a in unsafeCoerce# my

instance MonadTrans (CCT ans) where
  lift :: Monad m => m a -> CCT ans m a
  lift ma = CCT $ pure <$> ma

instance MonadIO m => MonadIO (CCT ans m) where
  liftIO :: IO a -> CCT ans m a
  liftIO io = lift (liftIO io)

runCC :: (forall ans. CC ans a) -> a
runCC (CC m) = case runRW# m of (# _, a #) -> a

runCCT :: Monad m => CCT ans m a -> m (CC ans a)
runCCT = unCCT

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