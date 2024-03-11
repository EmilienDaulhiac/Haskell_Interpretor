module Main where

import Test.HUnit
import TestSuite

main = do
  results <- runTestTT tests
  print results
