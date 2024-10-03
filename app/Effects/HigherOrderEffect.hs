module Effects.HigherOrderEffect where

import Data.Effect.TH (makeEffectF, makeEffectH)
import Data.Hefty.Extensible (type (<|), ForallHFunctor, type (<<|))
import Control.Effect (type (~>), type (<:), type (<<:))
import Control.Effect.ExtensibleFinal (type (:!!))
import Control.Effect.Hefty (interpretRec, runEff, interposeRec, interpretRecH, raise, raiseH, interposeRecH)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale, getCurrentTime)
import Data.Kind (Type)
import Control.Effect.Interpreter.Heftia.Reader (runReader)
import Control.Category ((>>>))
import Data.Effect.Reader (Local, LAsk, local, ask)

data Log a where
  Logging :: Text -> Log ()

makeEffectF [''Log]

logToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LLog ': r ~> eh :!! r
logToIO = interpretRec \(Logging msg) -> liftIO $ T.putStrLn msg

---------------------------------------------------------

data Time a where
  CurrentTime :: Time UTCTime

makeEffectF [''Time]

timeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTime ': r ~> eh :!! r
timeToIO = interpretRec \CurrentTime -> liftIO getCurrentTime

---------------------------------------------------------

logWithTime :: (Log <| ef, Time <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
logWithTime = interposeRec \(Logging msg) -> do
  t <- currentTime
  logging $ pack "[" <> iso8601 t <> pack "]" <> msg

iso8601 :: UTCTime -> Text
iso8601 t = T.take 23 (pack $ formatTime defaultTimeLocale "%FT%T.%q" t) <> pack "Z"

program :: IO ()
program =
  runEff . logToIO . timeToIO . logWithTime $ do
    logging $ pack "foo"
    logging $ pack "bar"
    logging $ pack "baz"

------------------------------------------

data LogChunk f (a :: Type) where
  LogChunk :: Text -> f a -> LogChunk f a

makeEffectH [''LogChunk]

runLogChunk :: ForallHFunctor eh => LogChunk ': eh :!! ef ~> eh :!! ef
runLogChunk = interpretRecH \(LogChunk _ m) -> m

logExample :: (LogChunk <<: m, Log <: m, MonadIO m) => m ()
logExample = do
  logging $ pack "out of chunk scope1 1"
  logging $ pack "out of chunk scope1 2"

  liftIO $ putStrLn "-------"

  logChunk (pack "scope2") do
    logging $ pack "in scope2 1"
    logging $ pack "in scope2 2"

  liftIO $ putStrLn "-------"

  logging $ pack "out of chunk scope1 3"
  logging $ pack "out of chunk scope1 4"
--------------------------------------------
data FileSystem a where
  Mkdir :: FilePath -> FileSystem ()
  WriteToFile :: FilePath -> String -> FileSystem ()

makeEffectF [''FileSystem]

runDymmyFS :: (IO <| r, ForallHFunctor eh) => eh :!! LFileSystem ': r ~> eh :!! r
runDymmyFS = interpretRec \case
    Mkdir path -> liftIO $ putStrLn $ "<runDummyFS> mkdir " <> path
    WriteToFile path content -> liftIO $ putStrLn $ "<runDummyFS> writeToTile" <> path <> " : " <> content
-----------------------------------
saveLogChunk ::
  (LogChunk <<| eh, Log <| ef, FileSystem <| eh, Time <| ef, ForallHFunctor eh) =>
  eh :!! ef ~> eh :!! ef
saveLogChunk = raise
  >>> raiseH
  >>> hookCreateDirectory
  >>> hookWriteFile
  >>> runReader @FilePath "./log/"

hookCreateDirectory :: (Local FilePath ': eh :!! LAsk FilePath ': ef) ~> (Local FilePath ': eh :!! LAsk FilePath ': ef)
hookCreateDirectory =
  interposeRecH \(LogChunk chunkName a) -> logChunk chunkName do
    chungBegingAt <- currentTime
    let dirName = unpack $ iso8601 chungBegingAt <> pack "-" <> chunkName
    local @FilePath (++ dirName ++ "/") do
      logChunkPath <- ask
      mkdir logChunkPath
      a