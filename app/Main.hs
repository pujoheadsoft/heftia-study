module Main where
import Data.Effect.TH (makeEffectF)
import Data.Hefty.Extensible (type (<|), ForallHFunctor)
import Control.Effect (type (~>), type (<:))
import Control.Effect.ExtensibleFinal (type (:!!))
import Control.Effect.Hefty (interpretRec, runEff, interposeRec)
import Control.Monad.IO.Class (liftIO)

data Teletype a where
  ReadTTY :: Teletype String
  WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]

{-
  IO <| r は、型レベルリストの変数 r の中に IO が含まれていることを示す。
  ForallHFunctor eh は、interpretRecを使えるようにする制約。これがないと interpret しか使えない。
  　interpretはすべてのEffectが解釈し終わった状態でないと使えないので、ハンドラが使用できるケースが限定的になる。
  a :!! b は、エフェクトフルなプログラムのモナドで、左側に高階エフェクト型の型レベルリストaと、右側に一階エフェクト型リストbを書く。
  　一階エフェクト型リストは、L + エフェクトのデータ型。これはmakeEffectFで生成される。LiftIns Teletypeでも同じ意味になる。
  ': はHaskell標準の型レベルリストのコンス演算子。
  ~> は、いわゆる自然変換を表す型演算子。
-}
teletypeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTeletype ': r ~> eh :!! r
teletypeToIO = interpretRec \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg

{-
  Effectの再解釈の例。
  interposeRec はEffectを途中で別の解釈に変更できる。
-}
strong :: (Teletype <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
strong = interposeRec \case
    ReadTTY -> readTTY
    WriteTTY msg -> writeTTY $ msg <> "!"

echo :: (Teletype <: m, Monad m) => m ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _ -> writeTTY i >> echo

program :: IO ()
program = runEff do
  liftIO $ putStrLn "Please enter something..."
  teletypeToIO echo

program2 :: IO ()
program2 = runEff do
  liftIO $ putStrLn "Please enter something..."
  teletypeToIO $ strong . strong $ echo

main :: IO ()
main = putStrLn "Hello, Haskell!"
