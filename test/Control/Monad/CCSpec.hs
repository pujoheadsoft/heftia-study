{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
module Control.Monad.CCSpec where

import Prelude hiding (either, any)
import Test.Hspec
import Control.Monad.CC
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.IO (evaluate)
import Test.MockCat

spec :: Spec
spec = do
  describe "moduleの説明に書かれていたコードのテスト" do
    it "pushPrompt" do
      {-
        このモジュールはさまざまな制御オペレーターを提供しており、ここに示されている例が適切なものを選ぶ手助けになることを願っています。
        最も基本的なものは、MonadDelimitedCont 型クラスに含まれる4つのオペレーターです。
        最初に紹介するのはもちろん newPrompt で、これは比較的分かりやすいでしょう。
        次に登場するのは pushPrompt で、計算を区切る基本的な操作です。
        他の制御オペレーターが存在しない場合、この操作は単なる何もしない操作（no-op）に過ぎません。

        runCC (newPrompt >>= \p -> pushPrompt p (pure "x")) 
      -}
      runCC (newPrompt >>= \p -> pushPrompt p (pure "x")) `shouldBe` "x"

    it "reset" do
      -- reset e = newPrompt >>= \p -> pushPrompt p (e p) なので次のように書き換えられる
      -- 今後同じコードはこの形で書く
      -- 他の制御オペレーターが存在しないため何もしない操作に等しい
      runCC (reset \_ -> pure "x") `shouldBe` "x"

    it "withSubCont" do
      {-
        withSubCont は部分継続をキャプチャすることを可能にする基本的な操作です。
        callCC とは異なり、withSubCont はキャプチャした区切られた継続を中断します。
        したがって次は、実行すると [1, 2] ではなく [] を結果として返します。
      -}
      runCC (reset \p -> (1:) <$> (2:) <$> withSubCont p (\_ -> return [])) `shouldBe` []

    it "pushSubCont" do
      {-
        最後の基本的な制御オペレーターは pushSubCont で
        これは withSubCont を使用してキャプチャされた部分継続を利用することを可能にします。
      -}
      runCC (reset \p -> (1:) <$> (2:) <$> withSubCont p (\k -> pushSubCont k (return []))) `shouldBe` [1, 2]
    
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

    it "継続を使って計算することができる" do
      let r = runCC $ reset $ \p -> do
            k <- shift p $ \k -> do
              l <- shift p $ \l -> k (l (pure 5))
              pure $ 2 * l
            pure $ 1 + k
      r * 3 `shouldBe` 33

    it "継続を使って計算することができる" do
      let r = runCC $ reset $ \p -> pushPrompt p do
            k <- shift0 p $ \k -> do
              l <- shift0 p $ \l -> k (l (pure 5))
              pure $ 2 * l
            pure $ 1 + k
      r * 3 `shouldBe` 33

    it "継続を取り出すことができる" do
      x <- runCCT $ reset \p -> do
        k <- shift p \k -> k (pure id)
        pure $ k (* 10)
      x 3 `shouldBe` 30