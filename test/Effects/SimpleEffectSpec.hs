{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeApplications #-}
module Effects.SimpleEffectSpec where
import Test.Hspec
import Control.Monad.Hefty
import Effects.SimpleEffect
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = do
  describe "単純なエフェクトフルプログラムのテスト" do
    it "echo - ユーザー入力が一回あったら、ユーザー入力が一度出力される" do
      readTTYStubFn <- createStubFn do 
        onCase $ pure @IO "something"
        onCase $ pure @IO ""

      writeTTYMock <- createMock $ any @String |> pure @IO ()
      
      let 
        run :: (IO <| r) => eh :!! Teletype ': r ~> eh :!! r
        run = interpret $ \case
          ReadTTY -> liftIO readTTYStubFn
          WriteTTY msg -> liftIO $ stubFn writeTTYMock msg

      r <- (runEff . run) echo
        
      r `shouldBe` ()
      writeTTYMock `shouldApplyTo` "something"

    it "echo - ユーザー入力が一回もなかったら、出力はされない" do
      readTTYStubFn <- createStubFn $ pure @IO ""

      writeTTYMock <- createMock $ any @String |> pure @IO ()
      
      let 
        run :: (IO <| r) => eh :!! Teletype ': r ~> eh :!! r
        run = interpret $ \case
          ReadTTY -> liftIO readTTYStubFn
          WriteTTY msg -> liftIO $ stubFn writeTTYMock msg

      r <- (runEff . run) echo
        
      r `shouldBe` ()
      writeTTYMock `shouldApplyTimesToAnything` (0 :: Int)