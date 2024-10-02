module Effects.HigherOrderEffect where

import Data.Effect.TH (makeEffectF)
import Data.Hefty.Extensible (type (<|), ForallHFunctor)
import Control.Effect (type (~>), type (<:))
import Control.Effect.ExtensibleFinal (type (:!!))
import Control.Effect.Hefty (interpretRec, runEff, interposeRec)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Time (UTCTime)

data Log a where
  Logging :: Text -> Log ()

makeEffectF [''Log]

logToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LLog ': r ~> eh :!! r
logToIO = interpretRec \(Logging msg) -> liftIO $ T.putStrLn msg

data Time a where
  CurrentTime :: Time UTCTime

makeEffectF [''Time]