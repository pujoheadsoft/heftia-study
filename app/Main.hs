module Main where

import Effects.SimpleEffect as Simple
import Effects.HigherOrderEffect as HigherOrder
import Effects.DelimitedContinuation as DelimitedContinuation
import DelimitedContinuation.Example as DCE1
import DelimitedContinuation.Example2 as DCE2
import DelimitedContinuation.CCDelcont.Example as CCDelcontExample
import DelimitedContinuation.CCDelcont.TreeExample as CCDelcontExample

main :: IO ()
main = do
  --Simple.program2
  -- HigherOrder.program
  -- HigherOrder.program2
  -- HigherOrder.program3
  -- DelimitedContinuation.program
  -- DCE1.main
  DCE2.program
  CCDelcontExample.program1
  
