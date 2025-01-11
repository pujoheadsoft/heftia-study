module Effects.SimpleEffect where

import Control.Monad.Hefty

data Teletype a where
  ReadTTY :: Teletype String
  WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]

{-
  IO <| r は、型レベルリストの変数 r の中に IO が含まれていることを示す。
  a :!! b は、エフェクトフルなプログラムのモナドで、左側に高階エフェクト型の型レベルリストaと、右側に一階エフェクト型リストbを書く。
  ': はHaskell標準の型レベルリストのコンス演算子。
  ~> は、いわゆる自然変換を表す型演算子。

  v0.3系まではForallHFunctorの制約やLTeletypeのような型レベルリストを使っていたが、v0.5.0では不要になったようだ。
-}
teletypeToIO :: (IO <| r) => eh :!! Teletype ': r ~> eh :!! r
teletypeToIO = interpret \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg

{-
interpretはv0.3.1とv0.5.0で挙動が変わっている。

v0.3.1のinterpret
  interpret ::
      forall e r ehs fr u c.
      (Freer c fr, Union u, HeadIns e) =>
      UnliftIfSingle e ~> Eff u fr ehs r ->
      Eff u fr '[] (e ': r) ~> Eff u fr ehs r
  interpret i = interpretAllE $ i . unliftInsIfSingle |+: injectF
  {-# INLINE interpret #-}

v0.5.0のinterpret
  interpret
      :: forall e ef eh
      . (e ~> Eff eh ef)
      -- ^ Effect handler
      -> Eff eh (e ': ef) ~> Eff eh ef
  interpret = reinterpret
  {-# INLINE interpret #-}

  reinterpret
      :: forall e ef' ef eh
      . (ef `IsSuffixOf` ef')
      => (e ~> Eff eh ef')
      -> Eff eh (e ': ef) ~> Eff eh ef'
  reinterpret f = reinterpretRecWith (stateless f)
  {-# INLINE reinterpret #-}
-}


{-
  Effectの再解釈の例。
  v0.3系では interposeRec を使わないとEffectを途中で別の解釈に変更できなかった。
-}
strong :: (Teletype <| ef) => eh :!! ef ~> eh :!! ef
strong = interpose \case
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