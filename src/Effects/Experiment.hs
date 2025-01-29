-- 色々試してみるためのモジュール
module Effects.Experiment where

import Control.Monad.Hefty

data X a where
  X :: Int -> Bool -> X String

makeEffectF [''X]

data Y a where
  Y :: Int -> Bool -> Y String

makeEffectF [''Y]

data Logging a where
  Logging :: String -> Logging ()

makeEffectF [''Logging]

-- transform :: forall e e' ef eh. (e ~> e') -> Eff eh (e ': ef) ~> Eff eh (e' ': ef)
transformXtoY :: eh :!! X ': ef ~> eh :!! Y ': ef
transformXtoY = transform \case
  X a b -> Y a b

-- translate :: forall e e' ef eh. (e' <| ef) => (e ~> e') -> Eff eh (e ': ef) ~> Eff eh ef
translateXtoY :: (Y <| ef) => eh :!! X ': ef ~> eh :!! ef
translateXtoY = translate hoge
  where 
    hoge :: (X ~> Y)
    hoge = \case
      X a b -> Y a b

-- rewrite :: forall e ef eh. (e <| ef) => (e ~> e) -> Eff eh ef ~> Eff eh ef
rewriteX :: eh :!! X ': ef ~> eh :!! X ': ef
rewriteX = rewrite \case
  X a b -> X (a + 1) (not b)