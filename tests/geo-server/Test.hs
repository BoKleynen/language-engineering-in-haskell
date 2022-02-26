module Main where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import GeoServerShallowTest
import GeoServerDeepTest

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
    [ geoServerShallowSpecs
    , geoServerDeepSpecs
    ]
  defaultMain (testGroup "All tests" [
      testGroup "Specs" specs
    ])

