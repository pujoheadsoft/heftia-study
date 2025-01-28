-- 色々試してみるためのモジュール
module Effects.Experiment where

import Control.Monad.Hefty

data X a where
  X :: X String

makeEffectF [''X]

data Y a where
  Y :: Y String

makeEffectF [''Y]

t :: eh :!! X ': ef ~> eh :!! Y ': ef
t = transform \case
  X -> Y
