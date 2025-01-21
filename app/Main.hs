module Main (main) where

import DelimitedContinuation.BuiltinCC as DCE1
import DelimitedContinuation.Example2 as DCE2
import DelimitedContinuation.CCDelcont.Example as CCDelcontExample
import DelimitedContinuation.CCDelcont.TreeIterator as CCDelcontTreeExample
import DelimitedContinuation.CCDelcont.BreadthFirstTraversal as CCDelcontBreadthFirstTraversal
import DelimitedContinuation.CCDelcont.ResumableParsing as CCDelcontResumableParsing

import Effects.SimpleEffect as Simple
import Effects.HigherOrderEffect as HigherOrder
import Effects.DelimitedContinuation as DelimitedContinuation

main :: IO ()
main = do
  -- Simple.program
  -- HigherOrder.program
  -- HigherOrder.program2
  -- HigherOrder.program3
  -- DelimitedContinuation.program
  DCE1.program
  -- DCE2.program
  -- DCE2.program2
  -- CCDelcontExample.program
  -- CCDelcontTreeExample.program
  -- CCDelcontBreadthFirstTraversal.program
  -- CCDelcontResumableParsing.program
