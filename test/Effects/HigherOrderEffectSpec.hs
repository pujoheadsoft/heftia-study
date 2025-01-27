{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeApplications #-}
module Effects.HigherOrderEffectSpec where
import Test.Hspec
import Control.Monad.Hefty
import Effects.SimpleEffect
import Test.MockCat
import Prelude hiding (any)
import Effects.HigherOrderEffect
import Data.Text (Text, pack, unpack)

spec :: Spec
spec = do
  describe "高階なエフェクトフルプログラムのテスト" do
    describe "logWithTime" do
      it "ユーザー入力が一回あったら、ユーザー入力が一度出力される" do
        writeTTYMock <- createMock $ any @Text |> pure @IO ()
        
        let 
          logToIO :: (IO <| r) => eh :!! Log ': r ~> eh :!! r
          logToIO = interpret \(Logging msg) -> liftIO $ stubFn writeTTYMock msg

          timeToIO :: (IO <| r) => eh :!! Time ': r ~> eh :!! r
          timeToIO = interpret \CurrentTime -> do
            liftIO $ pure @IO (read "2025-01-28 00:00:00 UTC")

        r <- (runEff . logToIO . timeToIO . logWithTime) do
          logging $ pack "foo"
          logging $ pack "bar"
          logging $ pack "baz"
          
        r `shouldBe` ()
        writeTTYMock `shouldApplyInOrder` [
            pack "[2025-01-28T00:00:00.000Z] foo"
          , pack "[2025-01-28T00:00:00.000Z] bar"
          , pack "[2025-01-28T00:00:00.000Z] baz" 
          ]

