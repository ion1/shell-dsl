module Main (main) where

import Test.Tasty

import qualified ShellDSLTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "shell-dsl"
    [ ShellDSLTests.tests
    ]
