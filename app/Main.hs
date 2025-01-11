module Main where

import Effects.SimpleEffect as Simple
import Effects.HigherOrderEffect as HigherOrder
import Effects.DelimitedContinuation as DelimitedContinuation

main :: IO ()
main = do
  --Simple.program2
  HigherOrder.program
  HigherOrder.program2
  HigherOrder.program3
  DelimitedContinuation.program
