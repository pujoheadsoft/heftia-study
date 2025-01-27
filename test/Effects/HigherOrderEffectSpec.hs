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
import Data.Time
import System.IO.Unsafe

toUTCTime :: String -> UTCTime
toUTCTime timeStr = unsafePerformIO $ do
  return $ parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" timeStr
  
spec :: Spec
spec = do
  describe "高階なエフェクトフルプログラムのテスト" do
    it "logWithTime 時刻つきでログが出力される" do
      readTTYStubFn <- createStubFn do 
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:00")
        onCase $ pure @IO (toUTCTime "2025-01-28 01:00:00")
        onCase $ pure @IO (toUTCTime "2025-01-28 02:00:00")
        

      logMock <- createMock $ any @Text |> pure @IO ()
      
      let 
        logToIO :: (IO <| r) => eh :!! Log ': r ~> eh :!! r
        logToIO = interpret \(Logging msg) -> liftIO $ stubFn logMock msg

        timeToIO :: (IO <| r) => eh :!! Time ': r ~> eh :!! r
        timeToIO = interpret \CurrentTime -> do
          liftIO readTTYStubFn

      r <- (runEff . logToIO . timeToIO . logWithTime) do
        logging $ pack "foo"
        logging $ pack "bar"
        logging $ pack "baz"
        
      r `shouldBe` ()
      logMock `shouldApplyInOrder` [
          pack "[2025-01-28T00:00:00.000Z] foo"
        , pack "[2025-01-28T01:00:00.000Z] bar"
        , pack "[2025-01-28T02:00:00.000Z] baz" 
        ]

