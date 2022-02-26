module GeoServerShallowTest (geoServerShallowSpecs) where

import Test.Hspec

import GeoServerShallow

geoServerShallowSpecs :: Spec
geoServerShallowSpecs = describe "GeoServerShallow" $ do
    spec_circle
    spec_square
    spec_intersection
    spec_translate

spec_circle :: Spec
spec_circle =
  describe "circle" $ do
    context "when the radius is 0" $ do
      let c = circle 0

      it "contains the origin" $
        (0, 0) `inRegion` c `shouldBe` True

      it "doesn't contain any other point" $
        (0.5, 0) `inRegion` c `shouldBe` False

    context "when the radius is positive" $ do
      let c = circle 1

      it "returns True if the point lies within the circle" $
        (0.5, 0.75) `inRegion` c `shouldBe` True

      it "returns False if the point lies outside the circle" $
        (1.5, 0.75) `inRegion` c `shouldBe` False

    context "when the radius is negative" $ do
      let c = circle (-1)

      it "returns True if the point lies within the circle" $
        (0.5, 0.75) `inRegion` c `shouldBe` True

      it "returns False if the point lies outside the circle" $
        (1.5, 0.75) `inRegion` c `shouldBe` False

spec_square :: Spec
spec_square =
  describe "square" $ do
    context "when the side has length 0" $ do
      let sq = square 0

      it "contains the origin" $
        (0, 0) `inRegion` sq `shouldBe` True

      it "doesn't contain any other point" $
        (0.5, 0) `inRegion` sq `shouldBe` False

    context "when the sides have a positive length" $ do
      let sq = square 2

      it "returns True if the point lies within the square" $
        (0.5, 0.75) `inRegion` sq `shouldBe` True

      it "returns False if the point lies outside the square" $
        (1.5, 0.75) `inRegion` sq `shouldBe` False

    context "when the sides have a negative length" $ do
      let sq = square (-2)

      it "returns False for all points" $
        (0.5, 0.75) `inRegion` sq `shouldBe` False


spec_intersection :: Spec
spec_intersection =
  describe "(/\\)" $ do
    let c1 = circle 2

    it "intersection with outside" $
      (0, 0) `inRegion` (c1 /\  outside c1) `shouldBe` False

    it "intersection with itself" $
      (0, 0) `inRegion` (c1 /\ c1) `shouldBe` True

spec_translate :: Spec
spec_translate =
  describe "translate" $ do
    let direction = (2,2)

    context "when the translated region is a circle" $ do
      it "returns True when the point lies in the translated circle" $
        (2,2) `inRegion` translate direction (circle 1) `shouldBe` True

      it "returns False when the point lies outside of the translated circle" $
        (0,0) `inRegion` translate direction (circle 1) `shouldBe` False
