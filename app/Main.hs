module Main where
import Data.Effect.TH (makeEffectF)
import Data.Hefty.Extensible (type (<|), ForallHFunctor)
import Control.Effect (type (~>))
import Control.Effect.ExtensibleFinal (type (:!!))
import Control.Effect.Hefty (interpretRec)
import Control.Monad.IO.Class (liftIO)

data Teletype a where
  ReadTTY :: Teletype String
  WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]

teletypeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTeletype ': r ~> eh :!! r
teletypeToIO = interpretRec \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg

main :: IO ()
main = putStrLn "Hello, Haskell!"
