module Effects.ExperimentSpec where

import Effects.Experiment
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)
import Control.Monad.Hefty
import Control.Category

spec :: Spec
spec = do
  describe "rwrite/transform/translateのテスト" do
    it "どれも使わない場合" do
      xStub <- createStubFn $ (100 :: Int) |> True |> "100"
      yStub <- createStubFn $ (200 :: Int) |> False |> "200"

      r <- ((interpret \(X i b) -> pure $ xStub i b)
        >>> (interpret \(Y i b) -> pure $ yStub i b)
        >>> runEff) do
          s1 <- x 100 True
          s2 <- y 200 False
          pure $ s1 <> ":" <> s2

      r `shouldBe` "100:200"

    it "rewriteは同じエフェクトならば書き換えることができる" do
      xStub <- createStubFn $ (101 :: Int) |> False |> "101"

      r <- (
        -- rewriteXはIntを+1してBoolを反転する関数
        rewriteX
        >>> (interpret \(X i b) -> pure $ xStub i b)
        >>> runEff) do
          -- xしか呼べない
          x 100 True

      r `shouldBe` "101"

    it "translateはエフェクトを別のエフェクトに変換できる。変換前のエフェクトフルプログラムは呼び出せる。" do
      yStub <- createStubFn do
        onCase $ (100 :: Int) |> True  |> "100"
        onCase $ (200 :: Int) |> False |> "200"

      r <- (
        -- Xを解釈する関数はない
        translateXtoY
        >>> (interpret \(Y i b) -> pure $ yStub i b)
        >>> runEff) do
          -- xもyも呼べる
          s1 <- x 100 True
          s2 <- y 200 False
          pure $ s1 <> ":" <> s2

      r `shouldBe` "100:200"

    it "translateはエフェクトを別のエフェクトに変換できるが変換後のエフェクトフルプログラムは呼び出せない。" do
      yStub <- createStubFn $ (100 :: Int) |> True |> "100"

      r <- (
        -- Xを解釈する関数はない
        transformXtoY
        >>> (interpret \(Y i b) -> pure $ yStub i b)
        >>> runEff) do
          -- 上でYに変換しているがxしか呼べない。
          x 100 True

      r `shouldBe` "100"
