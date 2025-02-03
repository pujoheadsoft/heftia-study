{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Effects.ExperimentSpec where

import Effects.Experiment
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)
import Control.Monad.Hefty
import Control.Category ( (>>>) )
import Control.Monad.Hefty.Except
    ( catch, throw, runExcept, Catch, Throw, runThrowIO, runCatchIO, runThrow )
import Control.Monad.Hefty.Resource
    ( bracket, bracketOnExcept, runResourceIO, Resource )
import Control.Monad.Hefty.Unlift ( runUnliftIO ) 
import Control.Exception (Exception, throwIO, evaluate, try)

spec :: Spec
spec = do
  {-
    https://github.com/sayo-hs/heftia/blob/master/heftia/src/Control/Monad/Hefty/Transform.hs
  -}
  describe "rwrite/transform/translateのテスト" do
    it "どれも使わない場合" do
      xStub <- createStubFn $ (100 :: Int) |> True |> "100"
      yStub <- createStubFn $ (200 :: Int) |> False |> "200"

      r <- ((interpret \(X i b) -> pure $ xStub i b)
        >>> (interpret \(Y i b) -> pure $ yStub i b)
        >>> runEff) do
          s1 <- x 100 True
          s2 <- y 200 False
          pure $ s1 <> ":" <> s2

      r `shouldBe` "100:200"

    it "rewriteは同じエフェクトならば書き換えることができる" do
      xStub <- createStubFn $ (101 :: Int) |> False |> "101"

      r <- (
        -- rewriteXはIntを+1してBoolを反転する関数
        rewriteX
        >>> (interpret \(X i b) -> pure $ xStub i b)
        >>> runEff) do
          -- xしか呼べない
          x 100 True

      r `shouldBe` "101"

    it "translateはエフェクトを別のエフェクトに変換できる。変換前のエフェクトフルプログラムは呼び出せる。" do
      yStub <- createStubFn do
        onCase $ (100 :: Int) |> True  |> "100"
        onCase $ (200 :: Int) |> False |> "200"

      r <- (
        -- Xを解釈する関数はない
        translateXtoY
        >>> (interpret \(Y i b) -> pure $ yStub i b)
        >>> runEff) do
          -- xもyも呼べる
          s1 <- x 100 True
          s2 <- y 200 False
          pure $ s1 <> ":" <> s2

      r `shouldBe` "100:200"

    it "translateはエフェクトを別のエフェクトに変換できるが変換後のエフェクトフルプログラムは呼び出せない。" do
      yStub <- createStubFn $ (100 :: Int) |> True |> "100"

      r <- (
        -- Xを解釈する関数はない
        transformXtoY
        >>> (interpret \(Y i b) -> pure $ yStub i b)
        >>> runEff) do
          -- 上でYに変換しているがxしか呼べない。
          x 100 True

      r `shouldBe` "100"

    it "高階" do
      xStub <- createStubFn do
        onCase $ (100 :: Int) |> True |> "100"
        onCase $ (300 :: Int) |> True |> "300"
      yStub <- createStubFn do
        onCase $ (200 :: Int) |> False |> "200"
        onCase $ (400 :: Int) |> False |> "400"

      r <- (
        (interpretH \(HigherOrderEffect m) -> m)
        >>> (interpret \(X i b) -> pure $ xStub i b)
        >>> (interpret \(Y i b) -> pure $ yStub i b)
        >>> runEff) higherOrderProgram

      r `shouldBe` "100:200 300:400"

  {-
    https://github.com/sayo-hs/data-effects/blob/master/data-effects/src/Data/Effect/Except.hs
    https://github.com/sayo-hs/heftia/blob/master/heftia-effects/src/Control/Monad/Hefty/Except.hs
  -}
  describe "catch/throwのテスト" do
    it "throwされない場合" do
      xStub <- createStubFn $ (100 :: Int) |> True |> "1"
      result <- (
        runExcept
        >>> (interpret \(X i b) -> pure $ xStub i b)
        >>> runEff) throwableProgram
      case result of
        Left (CustomError e) -> expectationFailure $ "Unexpected Left: " ++ show e
        Right r -> r `shouldBe` "1"

    it "throwされた場合" do
      xStub <- createStubFn $ (100 :: Int) |> True |> "100"
      result <- (
        runExcept
        >>> (interpret \(X i b) -> pure $ xStub i b)
        >>> runEff) throwableProgram
      case result of
        Left (CustomError e) -> e `shouldBe` "error"
        Right r -> expectationFailure $ "Unexpected Right: " ++ show r
  {-
     https://github.com/sayo-hs/data-effects/blob/master/data-effects/src/Data/Effect/Resource.hs
     https://github.com/sayo-hs/heftia/blob/master/heftia-effects/src/Control/Monad/Hefty/Resource.hs

     https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:bracket
     と同じセマンティクスを持つ。

     リソースを割り当て、使用し、その後始末をする。
     Bracket         :: f a -> (a -> f ()) -> (a -> f b) -> Resource f b
     リソースを割り当て、それを使用し、エラーが発生した場合はその後にクリーンアップする。
     BracketOnExcept :: f a -> (a -> f ()) -> (a -> f b) -> Resource f b

     f a は最初に実行する計算 (acquire)
     (a -> f ()) はリソースを解放する計算 (release/onException)
     (a -> f b) はリソースを使って行う計算 (thing)

     -- エフェクトUnliftIOとIOを制約に持ち、Resourceを除去する
     runResourceIO :: (UnliftIO <<| eh, IO <| ef) => Eff (Resource ': eh) ef ~> Eff eh ef
     -- MonadUnliftIOを制約に持ち、UnliftIOを除去する
     runUnliftIO   :: (MonadUnliftIO m) => Eff '[UnliftIO] '[m] ~> m

     runExcept :: Eff '[Catch e] (Throw e ': r) a -> Eff '[] r (Either e a)
     
  -}
  describe "resource" do
    it "bracket" do
      xStub <- createStubFn $ (100 :: Int) |> True |> "100"
      loggingMock <- createMock $ any @String |> pure @IO ()
      let
        p :: (Resource <<: m, X <: m, Logging <: m, Monad m) => m String
        p = bracket
          (do x 100 True)
          (\v -> do
            logging $ "release:" <> v
            pure ())
          (\v -> do
            logging $ "thing:" <> v
            pure v)
      r <- (
        (interpret \(X i b) -> pure $ xStub i b)
        >>> (interpret \(Logging msg) -> liftIO $ stubFn loggingMock msg)
        >>> runResourceIO
        >>> runUnliftIO
        ) p
      r `shouldBe` "100"
      loggingMock `shouldApplyInOrder` ["thing:100", "release:100"]

    it "bracketOnExcept(例外が発生しない場合)" do
      xStub <- createStubFn $ (100 :: Int) |> True |> "100"
      loggingMock <- createMock $ any @String |> pure @IO ()
      let
        p :: (Resource <<: m, X <: m, Logging <: m, Monad m) => m String
        p = bracketOnExcept
          (do x 100 True)
          (\v -> do
            logging $ "onException:" <> v
            pure ())
          (\v -> do
            logging $ "thing:" <> v
            pure v)
      r <- (
        (interpret \(X i b) -> pure $ xStub i b)
        >>> (interpret \(Logging msg) -> liftIO $ stubFn loggingMock msg)
        >>> runResourceIO -- Eff '[Resource, UnliftIO] '[IO] String -> Eff '[UnliftIO] '[IO] String
        >>> runUnliftIO   -- MonadUnliftIO m => Eff '[UnliftIO] '[m] ~> m
        ) p
      r `shouldBe` "100"
      loggingMock `shouldApplyInOrder` ["thing:100"]

    it "bracketOnExcept(例外が発生した場合)" do
      xStub <- createStubFn $ (100 :: Int) |> True |> "100"
      loggingMock <- createMock $ any @String |> pure @IO ()
      let
        p :: (Resource <<: m, X <: m, Logging <: m, IO <: m, Monad m, Throw CustomError <: m) => m String
        p = bracketOnExcept
          (do x 100 True)
          (\v -> do
            logging $ "onException!!!!!!!!!!!!!:" <> v
            pure ())
          (\v -> do
            logging $ "thing!!!!!!!!!!!!!!!!!!:" <> v
            --error "error"
            throw $ CustomError "error"
            pure v
            )
        y = (runThrowIO @CustomError
          >>> (interpret \(X i b) -> pure $ xStub i b)
          >>> (interpret \(Logging msg) -> liftIO $ putStrLn msg)
          >>> runResourceIO) p
      r <- try (runUnliftIO y)
      r `shouldBe` Left (CustomError "error")
      print "@@@@@@"

      --loggingMock `shouldApplyInOrder` ["thing:100"]

newtype CustomError = CustomError String
  deriving (Show, Eq)
  
instance Exception CustomError

throwableProgram :: (Catch CustomError <<: m, Throw CustomError <: m, X <: m, Monad m) => m String
throwableProgram = do
  catch
    (do
      r <- x 100 True
      if r == "100" then
        throw $ CustomError "error"
      else pure r)
    (throw @CustomError) -- 特に何もせずに例外を投げなおすだけのハンドラ
    -- ^ 例外ハンドラはいずれかの方法でエラー型を明示する必要がある
--  (\(CustomError e) -> throw $ CustomError e)
--  (\(e :: CustomError) -> throw e)
--  (\e -> throw (e :: CustomError))