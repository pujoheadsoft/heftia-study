{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Effects.ShiftResetSpec where

import Prelude hiding (either, any)
import Test.Hspec

import Data.Effect.ShiftReset ( reset, shiftF, shift, Reset, ShiftF, Shift_, embedF, ShiftKey, Shift', exit )
import Control.Monad.Hefty

import Control.Monad.Hefty.ShiftReset ( runReset, runShiftF, Shift, exitF, evalShift )
import Control.Category 
import Effects.HigherOrderEffect (Log, logging)
import Data.Text (pack)
import Control.Effect.Key (SendHOEBy)
import Control.Monad.Trans
import Test.MockCat

{-
  data Reset m (a :: Type) where
      Reset :: m a -> Reset m a
  makeEffectH [''Reset]

  data Shift' (ans :: Type) n m a where
      Shift
          :: forall ans n m a
          . ((a -> n ans) -> (forall x. m x -> n x) -> n ans)
          -> Shift' ans n m a
  makeKeyedEffect [] [''Shift']

  data ShiftF ans a where
      ShiftF :: forall ans a. ((a -> ans) -> ans) -> ShiftF ans a

  evalShift :: Eff '[Shift ans '[] ef] ef ans -> Eff '[] ef ans
  runShift  :: (a -> Eff '[] ef ans) -> Eff '[Shift ans '[] ef] ef a -> Eff '[] ef ans
  runShift_ :: Eff (Shift_ (Eff eh ef) ': eh) ef ~> Eff eh ef
  runShiftF :: Eff '[] (ShiftF (Eff '[] ef ans) ': ef) ans -> Eff '[] ef ans
  runReset  :: Eff (Reset ': eh) ef ~> Eff eh ef
  runEff    :: Monad m => Eff '[] '[m] ~> m
-}

spec :: Spec
spec = do
  describe "限定継続のテスト" do
    it "reset" do
      let 
        r :: (Reset <<: m, Monad m) => m String
        r = reset $ pure "x"
      x <- (runEff <<< runReset) r
      x `shouldBe` "x"

    it "継続を使って計算することができる" do
      let 
        program :: (Reset <<: m, SendHOEBy ShiftKey (Shift' String n) m, Monad m) => m String
        program = reset do
          x <- shift \k _ -> k "x"
          pure $ x <> "y"

      x <- (runEff <<< evalShift <<< runReset) program
      x `shouldBe` "xy"

    it "exitで継続を破棄することができる" do
      let 
        program :: (Reset <<: m, SendHOEBy ShiftKey (Shift' Int n) m, Monad m, Applicative n) => m Int
        program = reset do
          x <- exit $ 5 * 2
          pure $ 3 + x - 1 -- 継続部分 3 + [..] - 1は破棄される

      r <- (runEff <<< evalShift <<< runReset) program
      r `shouldBe` 10

    it "継続を使って実行順序を変更できる" do
      let
        program :: (Reset <<: m, SendHOEBy ShiftKey (Shift' Int n) m, Monad m, Functor n) => m Int
        program = reset do
          s <- shift \k _ -> (* 2) <$> k 3
          pure $ 1 + s -- 継続部分 3 + [..] - 1は破棄される

      r <- (runEff <<< evalShift <<< runReset) program
      r `shouldBe` 8 -- 2 * 3 + 1 に見えるが、(1 + 3) * 2 になる

    it "継続を複数回使うことができる" do

      printMock <- createMock $ any @String |> pure @IO ()

      let
        printStub :: String -> IO ()
        printStub = stubFn printMock

        -- 継続kを2回使う
        either a b = shift \k _ -> k a >> k b

        -- resetの中ではprintStubは一回の呼び出しに見える
        program = reset do
          x <- either "a" "b"
          liftIO $ printStub x
          pure ()

      r <- (runEff <<< evalShift <<< runReset) program
      r `shouldBe` ()

      -- -- printStubは2回呼ばれている
      printMock `shouldApplyInOrder` [ "a", "b" ]

