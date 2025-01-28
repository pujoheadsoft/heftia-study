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
import Control.Category ((>>>))

toUTCTime :: String -> UTCTime
toUTCTime timeStr = unsafePerformIO $ do
  return $ parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" timeStr
  
spec :: Spec
spec = do
  describe "一階のエフェクトフルプログラムのテスト" do
    it "時刻つきでログが出力される" do
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

  describe "高階のエフェクトフルプログラムのテスト" do
    it "logExample" do
      readTTYStubFn <- createStubFn do 
        onCase $ pure @IO (toUTCTime "2025-01-28 01:00:00")
        onCase $ pure @IO (toUTCTime "2025-01-28 02:00:00")
        onCase $ pure @IO (toUTCTime "2025-01-28 03:00:00")
        onCase $ pure @IO (toUTCTime "2025-01-28 04:00:00")
        onCase $ pure @IO (toUTCTime "2025-01-28 05:00:00")
        onCase $ pure @IO (toUTCTime "2025-01-28 06:00:00")
        
      logMock <- createMock $ any @Text |> pure @IO ()
      
      r <- (runLogChunk
          >>> logWithTime
          >>> (interpret \CurrentTime -> liftIO readTTYStubFn)
          >>> (interpret \(Logging msg) -> liftIO $ stubFn logMock msg)
          >>> runEff) logExample

      r `shouldBe` ()
      logMock `shouldApplyInOrder` [
          pack "[2025-01-28T01:00:00.000Z] out of chunk scope1 1"
        , pack "[2025-01-28T02:00:00.000Z] out of chunk scope1 2"
        , pack "[2025-01-28T03:00:00.000Z] in scope2 1"
        , pack "[2025-01-28T04:00:00.000Z] in scope2 2"
        , pack "[2025-01-28T05:00:00.000Z] out of chunk scope1 3"
        , pack "[2025-01-28T06:00:00.000Z] out of chunk scope1 4"
        ]

    -- it "" do
    --   readTTYStubFn <- createStubFn do 
    --     onCase $ pure @IO (toUTCTime "2025-01-28 01:00:00")
    --     onCase $ pure @IO (toUTCTime "2025-01-28 02:00:00")
    --     onCase $ pure @IO (toUTCTime "2025-01-28 03:00:00")
    --     onCase $ pure @IO (toUTCTime "2025-01-28 04:00:00")
    --     onCase $ pure @IO (toUTCTime "2025-01-28 05:00:00")
    --     onCase $ pure @IO (toUTCTime "2025-01-28 06:00:00")
        

    --   logMock <- createMock $ any @Text |> pure @IO ()
    --   mkdirMock <- createMock $ any @FilePath |> pure @IO ()
    --   writeToFileMock <- createMock $ any @FilePath |> any @String |> pure @IO ()
      
    --   let 
    --     logToIO :: (IO <| r) => eh :!! Log ': r ~> eh :!! r
    --     logToIO = interpret \(Logging msg) -> liftIO $ stubFn logMock msg

    --     timeToIO :: (IO <| r) => eh :!! Time ': r ~> eh :!! r
    --     timeToIO = interpret \CurrentTime -> liftIO readTTYStubFn

    --     runDymmyFS :: (IO <| r) => eh :!! FileSystem ': r ~> eh :!! r
    --     runDymmyFS = interpret \case
    --       Mkdir path -> liftIO $ stubFn mkdirMock path
    --       WriteToFile path content -> liftIO $ stubFn writeToFileMock path content

    --   r <- (
    --       saveLogChunk
    --       >>> runLogChunk
    --       >>> runDymmyFS
    --       >>> logWithTime
    --       >>> timeToIO
    --       >>> logToIO
    --       >>> runEff) logExample

    --   r `shouldBe` ()
    --   logMock `shouldApplyInOrder` [
    --       pack "[2025-01-28T01:00:00.000Z] out of chunk scope1 1"
    --     , pack "[2025-01-28T02:00:00.000Z] out of chunk scope1 2"
    --     , pack "[2025-01-28T03:00:00.000Z] in scope2 1"
    --     , pack "[2025-01-28T04:00:00.000Z] in scope2 2"
    --     , pack "[2025-01-28T05:00:00.000Z] out of chunk scope1 3"
    --     , pack "[2025-01-28T06:00:00.000Z] out of chunk scope1 4"
    --     ]