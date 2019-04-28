module Test.Smile.CliTest where

import Smile.Prelude
import Test.Tasty
import Test.Tasty.HUnit

test_something :: TestTree
test_something = testCase "something" (1 @?= 1)
