module Effects.HigherOrderEffect where

import Control.Monad.Hefty
import Control.Category ((>>>))
import Control.Monad.IO.Class (MonadIO)
import Data.Effect.Reader (Local, ask, local)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Effect.State (modify, get)
import Control.Monad (when)
import Control.Monad.Hefty.Reader (Ask, runReader)
import Control.Monad.Hefty.State (evalState)

data Log a where
  Logging :: Text -> Log ()

makeEffectF [''Log]

-- Logのハンドラ
logToIO :: (IO <| r) => eh :!! Log ': r ~> eh :!! r
logToIO = interpret \(Logging msg) -> liftIO $ T.putStrLn msg

---------------------------------------------------------

data Time a where
  CurrentTime :: Time UTCTime

makeEffectF [''Time]

-- Timeのハンドラ
timeToIO :: (IO <| r) => eh :!! Time ': r ~> eh :!! r
timeToIO = interpret \CurrentTime -> liftIO getCurrentTime

---------------------------------------------------------
-- LogとTimeのハンドラ
logWithTime :: (Log <| ef, Time <| ef) => eh :!! ef ~> eh :!! ef
logWithTime = interpose \(Logging msg) -> do -- interposeで再解釈
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

-- LogChunkのElaborator
runLogChunk :: LogChunk ': eh :!! ef ~> eh :!! ef
runLogChunk = interpretH \(LogChunk _ m) -> m

-- 高階なエフェクトフルプログラム
logExample :: (LogChunk <<: m, Log <: m, MonadIO m) => m ()
logExample = do
  logging $ pack "out of chunk scope1 1"
  logging $ pack "out of chunk scope1 2"

  logChunk (pack "scope2") do
    logging $ pack "in scope2 1"
    logging $ pack "in scope2 2"

  logging $ pack "out of chunk scope1 3"
  logging $ pack "out of chunk scope1 4"

--------------------------------------------
data FileSystem a where
  Mkdir :: FilePath -> FileSystem ()
  WriteToFile :: FilePath -> String -> FileSystem ()

makeEffectF [''FileSystem]

runDymmyFS :: (IO <| r) => eh :!! FileSystem ': r ~> eh :!! r
runDymmyFS = interpret \case
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
  (LogChunk <<| eh, Log <| ef, FileSystem <| ef, Time <| ef) =>
  eh :!! ef ~> eh :!! ef
saveLogChunk =
  raise        -- 引数の eh :!! ef が eh :!! (e1 ': ef) になる。(efに任意のエフェクトe1が加わってる)
    >>> raiseH -- 引数の eh :!! (e ': ef) が (e2 ': eh) :!! (e1 ': ef) になる。(ehに任意のエフェクトe2が加わっている)
    >>> hookCreateDirectory -- hooksCreateDirectoryの引数と返り値の型は丁度↑の型になっている。
    >>> hookWriteFile       -- hookWriteFileの引数も同様
    >>> runReader @FilePath "./log/"
  where
    hookCreateDirectory ::
      (Local FilePath ': eh :!! Ask FilePath ': ef)      -- LocalはReader系エフェクトの高階なlocalエフェクトに対応する型
        ~> (Local FilePath ': eh :!! Ask FilePath ': ef) -- Askは一階なaskエフェクトに対応する型
    hookCreateDirectory =
      interposeH \(LogChunk chunkName a) -> logChunk chunkName do
        chungBegingAt <- currentTime -- 一階のエフェクトリストefにTimeがあるからcurrentTimeが使える
        let dirName = unpack $ iso8601 chungBegingAt <> pack "-" <> chunkName -- Chunk名と現在時刻からディレクトリ名を作る
        local @FilePath (++ dirName ++ "/") do -- localは高階な操作なのでエフェクトを引数にとる
          logChunkPath <- ask                  -- 一階のエフェクトリストefにLAskがあるからaskが使える
          mkdir logChunkPath                   -- 一階のエフェクトリストefにFileSystemがあるからmkDirが使える
          a

    hookWriteFile ::
      (Local FilePath ': eh :!! Ask FilePath ': ef)
        ~> (Local FilePath ': eh :!! Ask FilePath ': ef)
    hookWriteFile =
      interpose \(Logging msg) -> do
        logChunkPath <- ask
        logAt <- currentTime
        writeToFile (unpack $ pack logChunkPath <> iso8601 logAt <> pack ".log") (unpack msg)
        logging msg

{-
  !! や + は :!! が型レベルリストを使うのに対する代替の記法
  eh や ef や r といった多相化されたリストの型変数が出現しない場合こう書ける
-}
runApp :: LogChunk !! (FileSystem + Time + Log + IO) ~> IO
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
 -> '[LogChunk] :!! Log ': ef
 ~> '[LogChunk] :!! Log ': ef
limitLogChunk n = reinterpretH $ elabLimitLogChunk n

{-
  raiseUnder: エフェクトリストの先頭の一つ下に新たな任意のエフェクト型を挿入する
                 eh :!! e1 ': ef
              ~> eh :!! e1 ': e2 ef

  e ~~> f: これは e f ~> f の型シノニム
            この例だと
            LogChunk ~~> '[LogChunk] :!! Log ': ef
            LogChunk ('[LogChunk] :!! Log ': ef) ~> ('[LogChunk] :!! Log ': ef)
            と同じ

  infix 2 ~~>
  -- | Type alias for a natural transformation style elaborator.
  type e ~~> f = e f ~> f

  v0.3.1ではこう書いていた
  Elab LogChunk ('[LogChunk] :!! LLog ': ef)
-}
elabLimitLogChunk
  :: Log <| ef
  => Int
  -> LogChunk ~~> '[LogChunk] :!! Log ': ef
elabLimitLogChunk n (LogChunk name a) =
  logChunk name do
    raise . raiseH $ limitLog $ runLogChunk $ limitLogChunk n a
  where
    limitLog
      :: Log <| ef
      => '[] :!! Log ': ef
      ~> '[] :!! ef
    limitLog a' =
      -- 初期値0でStateエフェクトをハンドル
      evalState @Int 0 $
        -- エフェクトの干渉を防ぐためinterposeではなく、interpretRecを使っている
        raiseUnder a' & interpret \(Logging msg) -> do
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