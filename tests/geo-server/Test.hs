module Main where

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

import GeoServerShallow

main :: IO ()
main = do
    specs <- concat <$> mapM testSpecs
        [ spec_intersection ]
    defaultMain (testGroup "All tests" [
        testGroup "Specs" specs
     ])

spec_intersection :: Spec
spec_intersection =
    describe "(/\\)" $ do
        let
            o = Point 0 0
            c1 = circle 2

        it "intersection with with outside" $
            o `inRegion` (c1 /\  outside c1) `shouldBe` False

        it "intersection with itself" $
            o `inRegion` (c1 /\ c1) `shouldBe` True
