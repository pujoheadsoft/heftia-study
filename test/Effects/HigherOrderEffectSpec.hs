{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeApplications #-}
module Effects.HigherOrderEffectSpec where
import Test.Hspec
import Control.Monad.Hefty
import Test.MockCat
import Prelude hiding (any)
import Effects.HigherOrderEffect
import Data.Text (Text, pack)
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
      
      r <- (logWithTime
        >>> (interpret \CurrentTime -> liftIO readTTYStubFn)
        >>> (interpret \(Logging msg) -> liftIO $ stubFn logMock msg)
        >>> runEff) do
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
    it "スコープを区切ってログ出力できる" do
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

    it "ログ出力しつつディレクトリやファイルも作成する" do
      readTTYStubFn <- createStubFn do 
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:01")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:02")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:03")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:04")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:05")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:06")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:07")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:08")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:09")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:10")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:11")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:12")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:13")
        

      logMock <- createMock $ any @Text |> pure @IO ()
      mkdirMock <- createMock $ any @FilePath |> pure @IO ()
      writeToFileMock <- createMock $ any @FilePath |> any @String |> pure @IO ()
      
      r <- (
          saveLogChunk
          >>> runLogChunk
          >>> (interpret \case
                Mkdir path -> liftIO $ stubFn mkdirMock $ "mkDir: " <> path
                WriteToFile path content -> liftIO $ stubFn writeToFileMock ("write: " <> path) content)
          >>> logWithTime
          >>> (interpret \CurrentTime -> liftIO readTTYStubFn)
          >>> (interpret \(Logging msg) -> liftIO $ stubFn logMock msg)
          >>> runEff) logExample

      r `shouldBe` ()
      mkdirMock `shouldApplyInOrder` [
          "mkDir: ./log/2025-01-28T00:00:05.000Z-scope2/"
        ]

      writeToFileMock `shouldApplyInOrder` [
          "write: ./log/2025-01-28T00:00:01.000Z.log" |> "out of chunk scope1 1"
        , "write: ./log/2025-01-28T00:00:03.000Z.log" |> "out of chunk scope1 2"
        , "write: ./log/2025-01-28T00:00:05.000Z-scope2/2025-01-28T00:00:06.000Z.log" |> "in scope2 1"
        , "write: ./log/2025-01-28T00:00:05.000Z-scope2/2025-01-28T00:00:08.000Z.log" |> "in scope2 2"
        , "write: ./log/2025-01-28T00:00:10.000Z.log" |> "out of chunk scope1 3"
        , "write: ./log/2025-01-28T00:00:12.000Z.log" |> "out of chunk scope1 4"
        ]

      logMock `shouldApplyInOrder` [
          pack "[2025-01-28T00:00:02.000Z] out of chunk scope1 1"
        , pack "[2025-01-28T00:00:04.000Z] out of chunk scope1 2"
        , pack "[2025-01-28T00:00:07.000Z] in scope2 1"
        , pack "[2025-01-28T00:00:09.000Z] in scope2 2"
        , pack "[2025-01-28T00:00:11.000Z] out of chunk scope1 3"
        , pack "[2025-01-28T00:00:13.000Z] out of chunk scope1 4"
        ]
      {-
        成形して時系列順に並べたもの
        write: ./log/2025-01-28T00:00:01.000Z.log                                 out of chunk scope1 1
        [2025-01-28T00:00:02.000Z]                                                out of chunk scope1 1
        write: ./log/2025-01-28T00:00:03.000Z.log                                 out of chunk scope1 2
        [2025-01-28T00:00:04.000Z]                                                out of chunk scope1 2
        mkDir: ./log/2025-01-28T00:00:05.000Z-scope2/
        write: ./log/2025-01-28T00:00:05.000Z-scope2/2025-01-28T00:00:06.000Z.log in scope2 1
        [2025-01-28T00:00:07.000Z]                                                in scope2 1
        write: ./log/2025-01-28T00:00:05.000Z-scope2/2025-01-28T00:00:08.000Z.log in scope2 2
        [2025-01-28T00:00:09.000Z]                                                in scope2 2
        write: ./log/2025-01-28T00:00:10.000Z.log                                 out of chunk scope1 3
        [2025-01-28T00:00:11.000Z]                                                out of chunk scope1 3
        write: ./log/2025-01-28T00:00:12.000Z.log                                 out of chunk scope1 4
        [2025-01-28T00:00:13.000Z]                                                out of chunk scope1 4
      -}

    it "limitLogChunkはログ出力の回数を制限することができる" do
      readTTYStubFn <- createStubFn do 
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:01")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:02")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:03")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:04")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:05")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:06")
        onCase $ pure @IO (toUTCTime "2025-01-28 00:00:07")     

      logMock <- createMock $ any @Text |> pure @IO ()
      
      r <- (
          limitLogChunk 2
          >>> subsume
          >>> runLogChunk
          >>> logWithTime
          >>> (interpret \CurrentTime -> liftIO readTTYStubFn)
          >>> (interpret \(Logging msg) -> liftIO $ stubFn logMock msg)
          >>> runEff) do
        logging $ pack "out of chunk scope1 1"
        logging $ pack "out of chunk scope1 2"

        logChunk (pack "scope2") do
          logging $ pack "in scope2 1"
          logging $ pack "in scope2 2"
          logging $ pack "in scope2 3"

        logging $ pack "out of chunk scope1 3"
        logging $ pack "out of chunk scope1 4"

      r `shouldBe` ()

      logMock `shouldApplyInOrder` [
          pack "[2025-01-28T00:00:01.000Z] out of chunk scope1 1"
        , pack "[2025-01-28T00:00:02.000Z] out of chunk scope1 2"
        , pack "[2025-01-28T00:00:03.000Z] in scope2 1"
        , pack "[2025-01-28T00:00:04.000Z] in scope2 2"
        , pack "[2025-01-28T00:00:05.000Z] Subsequent logs are ommited..."
        , pack "[2025-01-28T00:00:06.000Z] out of chunk scope1 3"
        , pack "[2025-01-28T00:00:07.000Z] out of chunk scope1 4"
        ]