module Effects.DelimitedContinuation where
import Data.Effect.TH (makeEffectF, makeEffectH)
import Data.Hefty.Extensible (ForallHFunctor, type (<|))
import Control.Effect.ExtensibleFinal ((:!!))
import Control.Effect (type (~>))
import Control.Effect.Hefty (interpretRec, Elab, interposeK, runEff, interpretRecH)
import Data.Function ((&))
import Control.Monad.IO.Class (liftIO)
{-
  限定継続 (delimited continuation)
-}

type ForkId = Int

-- 制御構造を非決定性計算的に分岐させるエフェクト
data Fork a where
  Fork :: Fork ForkId

makeEffectF [''Fork]

runForkSingle :: ForallHFunctor eh => eh :!! LFork ': r ~> eh :!! r
runForkSingle = interpretRec \Fork -> pure 0
--------------------------------------------
{-
   分岐の範囲をスコープで区切って限定するための高階エフェクト
   (分岐がスコープ内で収まっており、スコープの外側では分岐は継続しない、だから「限定」継続)
-}
data ResetFork f a where
  ResetFork :: Monoid w => f w -> ResetFork f w

makeEffectH [''ResetFork]

{-
  ResetFork の elaboration射(一階はハンドラというが高階の場合elaboratiionと呼ぶ)
  interposeK: 限定継続の取り出しを行う
-}
applyResetFork :: Fork <| r => Int -> Elab ResetFork ('[] :!! r)
applyResetFork numberOfFork (ResetFork m) =
  m & interposeK pure \resume Fork -> do
    -- 取り出した限定継続resumeを1からnumberOfForkにかけて呼び出し
    r <- mapM resume [1 .. numberOfFork]
    -- 各々の継続の結果をmconcatで結合して返す
    pure $ mconcat r

program :: IO ()
program =
  runEff
    . runForkSingle
    . interpretRecH (applyResetFork 4)
    $ do
        liftIO . putStrLn . (("[スコープ外] " ++) . show) =<< fork
        -- ここからが分岐のスコープ
        s <- resetFork $ do
          fid1 <- fork
          fid2 <- fork
          liftIO $ putStrLn $ "[`fork`の限定継続] Fork ID: " ++ show (fid1, fid2)
          pure $ show (fid1, fid2)
        liftIO $ putStrLn $ "スコープの終了. " ++ s
