module Effects.HigherOrderEffect where

import Data.Effect.TH (makeEffectF)
import Data.Hefty.Extensible (type (<|), ForallHFunctor)
import Control.Effect (type (~>), type (<:))
import Control.Effect.ExtensibleFinal (type (:!!))
import Control.Effect.Hefty (interpretRec, runEff, interposeRec)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

data Log a where
  Logging :: Text -> Log ()

makeEffectF [''Log]
