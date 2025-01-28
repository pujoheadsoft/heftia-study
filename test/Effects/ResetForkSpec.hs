{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeApplications #-}
module Effects.ResetForkSpec where
import Test.Hspec
import Control.Monad.Hefty
import Test.MockCat
import Prelude hiding (any)
import Effects.ResetFork
import Data.Text (Text, pack)
import Data.Time
import System.IO.Unsafe
import Control.Category ((>>>))
import Effects.HigherOrderEffect (logging, Log (..))

toUTCTime :: String -> UTCTime
toUTCTime timeStr = unsafePerformIO $ do
  return $ parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" timeStr
  
spec :: Spec
spec = do
  describe "高階のエフェクトフルプログラム(Reset/Fork)" do
    it "" do

      logMock <- createMock $ any @Text |> pure @IO ()
           
      r <- runEff
        . (interpret \(Logging msg) -> liftIO $ stubFn logMock msg)
        . (interpret \Fork -> pure 0)
        . interpretH (applyResetFork 4)
        $ do
            logging . pack . ("[スコープ外] " <>) . show =<< fork
            -- ここからが分岐のスコープ
            s <- resetFork $ do
              fid1 <- fork
              fid2 <- fork
              logging $ pack ("[`fork`の限定継続] Fork ID: " <> show (fid1, fid2))
              pure $ show (fid1, fid2)

            logging $ pack $ "スコープの終了. 結果: " ++ s
        
      r `shouldBe` ()
      logMock `shouldApplyInOrder` [
          pack "[スコープ外] 0"
        , pack "[`fork`の限定継続] Fork ID: (1,1)"
        , pack "[`fork`の限定継続] Fork ID: (1,2)"
        , pack "[`fork`の限定継続] Fork ID: (1,3)"
        , pack "[`fork`の限定継続] Fork ID: (1,4)"
        , pack "[`fork`の限定継続] Fork ID: (2,1)"
        , pack "[`fork`の限定継続] Fork ID: (2,2)"
        , pack "[`fork`の限定継続] Fork ID: (2,3)"
        , pack "[`fork`の限定継続] Fork ID: (2,4)"
        , pack "[`fork`の限定継続] Fork ID: (3,1)"
        , pack "[`fork`の限定継続] Fork ID: (3,2)"
        , pack "[`fork`の限定継続] Fork ID: (3,3)"
        , pack "[`fork`の限定継続] Fork ID: (3,4)"
        , pack "[`fork`の限定継続] Fork ID: (4,1)"
        , pack "[`fork`の限定継続] Fork ID: (4,2)"
        , pack "[`fork`の限定継続] Fork ID: (4,3)"
        , pack "[`fork`の限定継続] Fork ID: (4,4)"
        , pack "スコープの終了. 結果: (1,1)(1,2)(1,3)(1,4)(2,1)(2,2)(2,3)(2,4)(3,1)(3,2)(3,3)(3,4)(4,1)(4,2)(4,3)(4,4)"
        ]
