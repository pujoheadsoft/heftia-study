module Main (main) where

import DelimitedContinuation.Example as DCE1
import DelimitedContinuation.CCDelcont.Example as CCDelcontExample

import Effects.SimpleEffect as Simple
import Effects.HigherOrderEffect as HigherOrder
import Effects.DelimitedContinuation as DelimitedContinuation

main :: IO ()
main = do
  -- Simple.program
  HigherOrder.program
  HigherOrder.program2
  HigherOrder.program3
  DelimitedContinuation.program
  DCE1.program
  CCDelcontExample.program
