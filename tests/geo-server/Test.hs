module Main where

import Test.Tasty
import Test.Tasty.Hspec

import GeoServerShallowTest
import GeoServerDeepTest
import ParserTest

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
    [ geoServerShallowSpecs
    , geoServerDeepSpecs
    , geoServerParserSpecs
    ]
  defaultMain (testGroup "All tests"
    [ testGroup "Specs" specs
    , testGroup "properties"
        [ geoServerDeepQCProps
        , geoServerShallowQCProps
        , geoServerShallowHedgehogProps
        ]
    ])

