module Main (main) where
import Effects.SimpleEffect as Simple
import Effects.HigherOrderEffect as HigherOrder
import Effects.ResetFork as ResetFork

main :: IO ()
main = do
  Simple.program
  HigherOrder.program
  HigherOrder.program2
  HigherOrder.program3
  ResetFork.program

