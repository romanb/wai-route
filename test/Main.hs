module Main where

import Test.Tasty

import qualified Test.Network.Wai.Route as Route

main :: IO ()
main = defaultMain $ testGroup "Tests" [ Route.tests ]
