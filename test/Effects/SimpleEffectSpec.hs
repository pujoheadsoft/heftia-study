{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeApplications #-}
module Effects.SimpleEffectSpec where
import Test.Hspec
import Control.Monad.Hefty
import Effects.SimpleEffect
import Test.MockCat
import Control.Monad.Hefty.State (put)

spec :: Spec
spec = do
  describe "単純なエフェクトフルプログラムのテスト" do
    it "echo" do
      readTTYStubFn <- createStubFn do 
        onCase $ pure @IO "output"
        onCase $ pure @IO ""

      writeTTYMock <- createMock $ "output" |> ()
      
      let 
        run :: (IO <| r) => eh :!! Teletype ': r ~> eh :!! r
        run = interpret $ \case
          ReadTTY -> liftIO do
            value <- readTTYStubFn
            putStrLn $ "ReadTTY:" <> value
            pure value
          WriteTTY msg -> do
            liftIO $ putStrLn $ "呼ばれたよ:"<> msg
            pure $ stubFn writeTTYMock msg

      r <- (runEff . run) echo
        
      r `shouldBe` ()

      --writeTTYMock `shouldApplyTo` "output"