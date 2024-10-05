module Effects.DelimitedContinuation where
import Data.Effect.TH (makeEffectF, makeEffectH)
import Data.Hefty.Extensible (ForallHFunctor, type (<|))
import Control.Effect.ExtensibleFinal ((:!!))
import Control.Effect (type (~>))
import Control.Effect.Hefty (interpretRec, Elab, interposeK)
import Data.Function ((&))
{-
  限定継続 (delimited continuation)
-}

type ForkId = Int

data Fork a where
  Fork :: Fork ForkId

makeEffectF [''Fork]

runForkSingle :: ForallHFunctor eh => eh :!! LFork ': r ~> eh :!! r
runForkSingle = interpretRec \Fork -> pure 0
--------------------------------------------
data ResetFork f a where
  ResetFork :: Monoid w => f w -> ResetFork f w

makeEffectH [''ResetFork]

applyResetFork :: Fork <| r => Int -> Elab ResetFork ('[] :!! r)
applyResetFork numberOfFork (ResetFork m) =
  m & interposeK pure \resume Fork -> do
    r <- mapM resume [1 .. numberOfFork]
    pure $ mconcat r