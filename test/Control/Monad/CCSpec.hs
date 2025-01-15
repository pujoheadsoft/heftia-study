{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Control.Monad.CCSpec where

import Prelude hiding (either, any)
import Test.Hspec
import Control.Monad.CC
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.IO (evaluate)
import Test.MockCat

spec :: Spec
spec = do
  describe "限定継続のテスト" do
    it "継続を使って計算することができる" do
      let r = runCC $ reset $ \p -> do
            s <- shift p $ \k -> k $ pure (5 * 2)
            pure $ 3 + s - 1 -- 3 + 10 - 1
      r `shouldBe` 12

    it "継続を破棄することができる" do
      let r = runCC $ reset $ \p -> do
            s <- shift p $ \_ -> pure $ 5 * 2
            pure $ 3 + s - 1 -- 継続部分 3 + [..] - 1は破棄される
      r `shouldBe` 10

    it "継続を使って実行順序を変更できる" do
      let r = runCC $ reset $ \p -> do
            s <- shift p \k -> (* 2) <$> k (pure 3)
            pure $ 1 + s
      r `shouldBe` 8 -- 2 * 3 + 1 に見えるが、(1 + 3) * 2 になる

    it "継続を複数回使うことができる" do

      printMock <- createMock $ any @String |> pure @IO ()

      let
        printStub = stubFn printMock

        -- 継続kを2回使う
        either p a b = shift p \k -> k a >> k b

        -- resetの中ではprintStubは一回の呼び出しに見える
        r :: (MonadDelimitedCont p s m, MonadIO m) => m ()
        r = reset \p -> do
          x <- either p (pure "a") (pure "b")
          liftIO $ printStub x
          pure ()

      (runCCT r >>= evaluate) `shouldReturn` ()

      -- printStubは2回呼ばれている
      printMock `shouldApplyInOrder` [ "a", "b" ]
