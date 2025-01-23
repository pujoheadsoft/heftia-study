{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
module DelimitedContinuation.BuiltinCCSpec where

import Prelude hiding (either, any)
import Test.Hspec
import DelimitedContinuation.BuiltinCC
import Test.MockCat
import Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = do
  describe "基本的なオペレーターのテスト" do
    it "pushPrompt" do
      runCC (newPrompt >>= \p -> pushPrompt p (pure "x")) `shouldBe` "x"

    it "reset" do
      runCC (reset \_ -> pure "x") `shouldBe` "x"

    it "withSubCont" do
      runCC (reset \p -> (1:) <$> (2:) <$> withSubCont p (\_ -> return [])) `shouldBe` []

    it "pushSubCont" do
      runCC (reset \p -> (1:) <$> (2:) <$> withSubCont p (\k -> pushSubCont k (return []))) `shouldBe` [1, 2]

  describe "限定継続のテスト" do
    it "継続を使って計算することができる" do
      let r = runCC $ reset $ \p -> do
            s <- shift p $ \k -> k (5 * 2)
            pure $ 3 + s - 1 -- 3 + 10 - 1
      r `shouldBe` 12

    it "継続を破棄することができる(shift版)" do
      let r = runCC $ reset $ \p -> do
            s <- shift p $ \_ -> pure $ 5 * 2
            pure $ 3 + s - 1 -- 継続部分 3 + [..] - 1は破棄される
      r `shouldBe` 10

    it "pushPromptで計算を区切った中のshiftで継続を破棄しても外側の継続は破棄されない" do
      let r = runCC $ reset $ \p -> do
            s <- pushPrompt p $ shift p $ \_ -> pure $ 5 * 2
            pure $ 3 + s - 1 -- 継続部分 3 + [..] - 1は破棄されない
      r `shouldBe` 12

    it "pushPromptで計算を区切った中のshiftで継続を破棄しても外側の継続は破棄されないが、内側の継続は破棄される" do
      let r = runCC $ reset $ \p -> do
            -- 継続 [..] * 10 は破棄される
            s <- pushPrompt p ((* 10) <$> shift p (\_ -> pure $ 5 * 2))
            pure $ 3 + s - 1 -- 継続部分 3 + [..] - 1は破棄さない
      r `shouldBe` 12

    it "継続を破棄することができる(abort版)" do
      let r = runCC $ reset $ \p -> do
            s <- abort p $ pure $ 5 * 2
            pure $ 3 + s - 1 -- 継続部分 3 + [..] - 1は破棄される
      r `shouldBe` 10

    it "継続を使って実行順序を変更できる" do
      let r = runCC $ reset $ \p -> do
            s <- shift p \k -> (* 2) <$> k 3
            pure $ 1 + s
      r `shouldBe` 8 -- 2 * 3 + 1 に見えるが、(1 + 3) * 2 になる

    it "継続を複数回使うことができる" do

      printMock <- createMock $ any @String |> pure @IO ()

      let
        printStub :: String -> IO ()
        printStub = stubFn printMock

        -- 継続kを2回使う
        either p a b = shift p \k -> k a >> k b

        -- resetの中ではprintStubは一回の呼び出しに見える
        r =  reset \p -> do
          x <- either p "a" "b"
          liftIO $ printStub x
          pure ()

      runCC r `shouldBe` ()

      -- -- printStubは2回呼ばれている
      printMock `shouldApplyInOrder` [ "a", "b" ]

    it "継続を使って計算することができる" do
      let r = runCC $ reset $ \p -> do
            k <- shift p $ \k -> do
              l <- shift p $ \l -> k (l 5)
              pure $ 2 * l
            (+ 1) <$> k
      (r * 3) `shouldBe` 33

    it "継続を使って計算することができる" do
      let r = runCC $ reset $ \p -> pushPrompt p do
            k <- shift0 p $ \k -> do
              l <- shift0 p $ \l -> k (l 5)
              pure $ 2 * l
            (+ 1) <$> k
      r * 3 `shouldBe` 33

    it "継続を取り出すことができる" do
      let 
        r = runCC $ reset \p -> do
          k <- shift p \k -> k id
          pure $ (+ 3) <$> k <$> (* 10)
      r 3 `shouldBe` 33

    describe "shiftとshift0の違い" do
      it "pushPromptで区切らない場合同じ結果になる" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> shift  p (\_ -> pure 2))
          r2 = runCC $ reset (\p -> (+ 1) <$> shift0 p (\_ -> pure 2))

        r1 `shouldBe` 2
        r2 `shouldBe` 2

      it "pushPromptで区切った場合も同じ結果になる" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (shift  p (\_ -> pure 2)))
          r2 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (shift0 p (\_ -> pure 2)))

        r1 `shouldBe` 3
        r2 `shouldBe` 3

      it "pushPromptで区切った中でネストした場合、shift0は外側の継続が破棄される" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (shift  p (\_ -> shift  p (\_ -> pure 2))))
          r2 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (shift0 p (\_ -> shift0 p (\_ -> pure 2))))
          -- ↑ r2は継続 [..] + 1 が破棄される。↓のようにネストした中でpushPromptすればshift0でも外側の継続が破棄さない
          r3 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (shift0 p (\_ -> pushPrompt p $ shift0 p (\_ -> pure 2))))
        r1 `shouldBe` 3
        r2 `shouldBe` 2
        r3 `shouldBe` 3

    describe "controlとcontrol0の違い" do
      it "pushPromptで区切らない場合同じ結果になる" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> control  p (\_ -> pure 2))
          r2 = runCC $ reset (\p -> (+ 1) <$> control0 p (\_ -> pure 2))

        r1 `shouldBe` 2
        r2 `shouldBe` 2

      it "pushPromptで区切った場合も同じ結果になる" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (control  p (\_ -> pure 2)))
          r2 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (control0 p (\_ -> pure 2)))

        r1 `shouldBe` 3
        r2 `shouldBe` 3

      it "pushPromptで区切った中でネストした場合、control0は外側の継続が破棄される" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (control  p (\_ -> control  p (\_ -> pure 2))))
          r2 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (control0 p (\_ -> control0 p (\_ -> pure 2))))
          r3 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (control0 p (\_ -> pushPrompt p $ control0 p (\_ -> pure 2))))
        r1 `shouldBe` 3
        r2 `shouldBe` 2
        r3 `shouldBe` 3

    describe "shiftとcontrolの違い" do
      it "shift/control" do
        let
          rs = runCC $ reset (\p -> shift   p (\k -> (1:) <$> k []) >>= \y -> shift   p (\_ -> pure y))
          rc = runCC $ reset (\p -> control p (\k -> (1:) <$> k []) >>= \y -> control p (\_ -> pure y))

        rs `shouldBe` [1]
        rc `shouldBe` []

      it "shift/shift0/control/control0" do
        let
          s1 = runCC $ reset (\p -> (1:) <$> pushPrompt p (shift    p (\_ -> shift   p (\k -> (2:) <$> k []) >>= \y -> shift   p (\_ -> pure y))))
          s2 = runCC $ reset (\p -> (1:) <$> pushPrompt p (shift0   p (\_ -> shift   p (\k -> (2:) <$> k []) >>= \y -> shift   p (\_ -> pure y))))
          c1 = runCC $ reset (\p -> (1:) <$> pushPrompt p (control  p (\_ -> control p (\k -> (2:) <$> k []) >>= \y -> control p (\_ -> pure y))))
          c2 = runCC $ reset (\p -> (1:) <$> pushPrompt p (control0 p (\_ -> control p (\k -> (2:) <$> k []) >>= \y -> control p (\_ -> pure y))))

        s1 `shouldBe` [1, 2] -- shiftは外側も内側も破棄されない
        s2 `shouldBe` [2] -- shift0は外側の継続が破棄されるが、内側は破棄されない
        c1 `shouldBe` [1] -- controlは外側の継続が破棄されないが、内側は破棄される
        c2 `shouldBe` []  -- control0は外側も内側も破棄される
