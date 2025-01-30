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

data HigherOrderEffect f (a :: Type) where
  HigherOrderEffect :: f a -> HigherOrderEffect f a

makeEffectH [''HigherOrderEffect]

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

higherOrderProgram :: (HigherOrderEffect <<: m, X <: m, Y <: m, Monad m) => m String
higherOrderProgram = do
  h1 <- higherOrderEffect do
    r1 <- x 100 True
    r2 <- y 200 False
    pure $ r1 <> ":" <> r2
  h2 <- higherOrderEffect do
    r1 <- x 300 True
    r2 <- y 400 False
    pure $ r1 <> ":" <> r2
  pure $ h1 <> " " <> h2

instance Show (X a) where
  show (X a b) = "X " ++ show a ++ " " ++ show b
instance Eq (X a) where
  X a b == X a' b' = a == a' && b == b'

instance Show (Y a) where
  show (Y a b) = "Y " ++ show a ++ " " ++ show b
instance Eq (Y a) where
  Y a b == Y a' b' = a == a' && b == b'