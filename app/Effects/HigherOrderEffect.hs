module Effects.HigherOrderEffect where

import Control.Category ((>>>))
import Control.Effect (type (<:), type (<<:), type (~>))
import Control.Effect.ExtensibleFinal (type (:!!), type (!!))
import Control.Effect.Hefty (interposeRec, interposeRecH, interpretRec, interpretRecH, raise, raiseH, runEff, reinterpretRecH, Elab, raiseUnder, subsume)
import Control.Effect.Interpreter.Heftia.Reader (runReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Effect.Reader (LAsk, Local, ask, local)
import Data.Effect.TH (makeEffectF, makeEffectH)
import Data.Hefty.Extensible (ForallHFunctor, type (<<|), type (<|))
import Data.Kind (Type)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Free.Sum (type (+))
import Control.Effect.Interpreter.Heftia.State (evalState)
import Data.Effect.State (modify, get)
import Control.Monad (when)
import Data.Function ((&))

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
  logging $ pack "[" <> iso8601 t <> pack "] " <> msg

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

runLogChunk :: (ForallHFunctor eh) => LogChunk ': eh :!! ef ~> eh :!! ef
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
  Mkdir path -> liftIO $ putStrLn $ "<runDummyFS> mkdir: " <> path
  WriteToFile path content -> liftIO $ putStrLn $ "<runDummyFS> writeToTile: " <> path <> " : " <> content

-----------------------------------
{-
  <<| は <| の高階版
  raise:  一階エフェクトリストの先頭に新たな任意のエフェクトを追加
             eh :!! ef
          ~> eh :!! e ': ef

  raiseH: 高階エフェクトリストの先頭に新たな任意のエフェクトを追加
                  eh :!! ef
          ~> e ': eh :!! ef
-}
saveLogChunk ::
  forall eh ef.
  (LogChunk <<| eh, Log <| ef, FileSystem <| ef, Time <| ef, ForallHFunctor eh) =>
  eh :!! ef ~> eh :!! ef
saveLogChunk =
  raise        -- 引数の eh :!! ef が eh :!! (e1 ': ef) になる。(efに任意のエフェクトe1が加わってる)
    >>> raiseH -- 引数の eh :!! (e ': ef) が (e2 ': eh) :!! (e1 ': ef) になる。(ehに任意のエフェクトe2が加わっている)
    >>> hookCreateDirectory -- hooksCreateDirectoryの引数と返り値の型は丁度↑の型になっている。
    >>> hookWriteFile       -- hookWriteFileの引数も同様
    >>> runReader @FilePath "./log/"
  where
    hookCreateDirectory ::
      (Local FilePath ': eh :!! LAsk FilePath ': ef)      -- LocalはReader系エフェクトの高階なlocalエフェクトに対応する型
        ~> (Local FilePath ': eh :!! LAsk FilePath ': ef) -- Askは一階なaskエフェクトに対応する型
    hookCreateDirectory =
      interposeRecH \(LogChunk chunkName a) -> logChunk chunkName do
        chungBegingAt <- currentTime -- 一階のエフェクトリストefにTimeがあるからcurrentTimeが使える
        let dirName = unpack $ iso8601 chungBegingAt <> pack "-" <> chunkName -- Chunk名と現在時刻からディレクトリ名を作る
        local @FilePath (++ dirName ++ "/") do -- localは高階な操作なのでエフェクトを引数にとる
          logChunkPath <- ask                  -- 一階のエフェクトリストefにLAskがあるからaskが使える
          mkdir logChunkPath                   -- 一階のエフェクトリストefにFileSystemがあるからmkDirが使える
          a

    hookWriteFile ::
      (Local FilePath ': eh :!! LAsk FilePath ': ef)
        ~> (Local FilePath ': eh :!! LAsk FilePath ': ef)
    hookWriteFile =
      interposeRec \(Logging msg) -> do
        logChunkPath <- ask
        logAt <- currentTime
        writeToFile (unpack $ pack logChunkPath <> iso8601 logAt <> pack ".log") (unpack msg)
        logging msg

{-
  !! や + は :!! が型レベルリストを使うのに対する代替の記法
  eh や ef や r といった多相化されたリストの型変数が出現しない場合こう書ける
-}
runApp :: LogChunk !! FileSystem + Time + Log + IO ~> IO
runApp =
  runLogChunk
    >>> runDymmyFS
    >>> logWithTime
    >>> timeToIO
    >>> logToIO
    >>> runEff

program2 :: IO ()
program2 = runApp . saveLogChunk $ logExample
------------------------------------------------
{-
  スコープ内でログがn回以上投げられた場合、n回以降は省略し、省略されたことをログに出すという再解釈を行うフック
-}
limitLogChunk
 :: Log <| ef
 => Int
 -> '[LogChunk] :!! LLog ': ef
 ~> '[LogChunk] :!! LLog ': ef
limitLogChunk n = reinterpretRecH $ elabLimitLogChunk n

{-
  raiseUnder: エフェクトリストの先頭の一つ下に新たな任意のエフェクト型を挿入する
                 eh :!! e1 ': ef
              ~> eh :!! e1 ': e2 ef

  Elab e f: これは e f ~> f の型シノニム
            この例だと
            Elab LogChunk ('[LogChunk] :!! LLog ': ef) は
            LogChunk ('[LogChunk] :!! LLog ': ef) ~> ('[LogChunk] :!! LLog ': ef)
            と同じ
-}
elabLimitLogChunk
  :: Log <| ef
  => Int
  -> Elab LogChunk ('[LogChunk] :!! LLog ': ef)
elabLimitLogChunk n (LogChunk name a) =
  logChunk name do
    raise . raiseH $ limitLog $ runLogChunk $ limitLogChunk n a
  where
    limitLog
      :: Log <| ef
      => '[] :!! LLog ': ef
      ~> '[] :!! ef
    limitLog a' =
      -- 初期値0でStateエフェクトをハンドル
      evalState @Int 0 $
        -- エフェクトの干渉を防ぐためinterposeRecではなく、interpretRecを使っている
        raiseUnder a' & interpretRec \(Logging msg) -> do
          count <- get
          when (count < n) do -- 条件を満たすときだけログ出力
            logging msg
            when (count == n - 1) do -- limitまできたらログ出力
              logging $ pack "Subsequent logs are ommited..."
            
            modify @Int (+ 1) -- インクリメント

{-
  subsume: 先頭のエフェクトをそれよりも下位へと送信する

  subsume
    :: (e <| ef, ForallHFunctor eh)
    => eh :!! LiftIns e ': ef
    ~> eh :!! ef
  subsume = interpretRec sendIns
-}
program3 :: IO ()
program3 = runApp . subsume . limitLogChunk 2 $ logExample