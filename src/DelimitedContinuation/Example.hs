{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}

module DelimitedContinuation.Example where

import GHC.Exts
import GHC.IO

type role CC nominal representational

newtype CC ans a = CC (State# RealWorld -> (# State# RealWorld, a #))
  deriving (Functor, Applicative, Monad) via IO

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

shift :: Prompt ans a -> ((b -> CC ans a) -> CC ans a) -> CC ans b
shift p f = withSubCont p (\sk -> pushPrompt p (f (pushPrompt p . pushSubCont sk . pure)))

program :: IO ()
program = do
  let computation = do
        p <- newPrompt
        pushPrompt p $ do
          (3 *)
            <$> shift
              p
              ( \k -> do
                  x <- k 5
                  y <- k x
                  pure $ 1 + y
              )
  print (runCC computation)