{-# LANGUAGE BlockArguments #-}

module GeoServerShallowTest (geoServerShallowSpecs, geoServerShallowQCProps, geoServerShallowHedgehogProps) where

import Control.Applicative
import Hedgehog ( (===), assert, forAll, property, Gen )
import Test.Hspec
import Test.QuickCheck((==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

import qualified Test.Tasty.QuickCheck as QC
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import GeoServerShallow


geoServerShallowSpecs :: Spec
geoServerShallowSpecs = describe "GeoServerShallow" do
    spec_circle
    spec_square
    spec_intersection
    spec_translate

spec_circle :: Spec
spec_circle =
  describe "circle" do
    context "when the radius is 0" do
      let c = circle 0

      it "contains the origin" $
        (0, 0) `inRegion` c `shouldBe` True

      it "doesn't contain any other point" $
        (0.5, 0) `inRegion` c `shouldBe` False

    context "when the radius is positive" do
      let c = circle 1

      it "returns True if the point lies within the circle" $
        (0.5, 0.75) `inRegion` c `shouldBe` True

      it "returns False if the point lies outside the circle" $
        (1.5, 0.75) `inRegion` c `shouldBe` False

    context "when the radius is negative" do
      let c = circle (-1)

      it "returns True if the point lies within the circle" $
        (0.5, 0.75) `inRegion` c `shouldBe` True

      it "returns False if the point lies outside the circle" $
        (1.5, 0.75) `inRegion` c `shouldBe` False

spec_square :: Spec
spec_square =
  describe "square" do
    context "when the side has length 0" do
      let sq = square 0

      it "contains the origin" $
        (0, 0) `inRegion` sq `shouldBe` True

      it "doesn't contain any other point" $
        (0.5, 0) `inRegion` sq `shouldBe` False

    context "when the sides have a positive length" do
      let sq = square 2

      it "returns True if the point lies within the square" $
        (0.5, 0.75) `inRegion` sq `shouldBe` True

      it "returns False if the point lies outside the square" $
        (1.5, 0.75) `inRegion` sq `shouldBe` False

    context "when the sides have a negative length" do
      let sq = square (-2)

      it "returns False for all points" $
        (0.5, 0.75) `inRegion` sq `shouldBe` False


spec_intersection :: Spec
spec_intersection =
  describe "(/\\)" do
    let c1 = circle 2

    it "intersection with outside" $
      (0, 0) `inRegion` (c1 /\  outside c1) `shouldBe` False

    it "intersection with itself" $
      (0, 0) `inRegion` (c1 /\ c1) `shouldBe` True

spec_translate :: Spec
spec_translate =
  describe "translate" do
    let direction = (2,2)

    context "when the translated region is a circle" do
      it "returns True when the point lies in the translated circle" $
        (2,2) `inRegion` translate direction (circle 1) `shouldBe` True

      it "returns False when the point lies outside of the translated circle" $
        (0,0) `inRegion` translate direction (circle 1) `shouldBe` False

geoServerShallowQCProps :: TestTree
geoServerShallowQCProps = testGroup "quickcheck test"
  [ QC.testProperty "points are inside a region or outside a reion"
      \(r, p) -> p `inRegion` r || p `inRegion` outside r

  , QC.testProperty "points can't be inside and outside a region"
      \(r, p) -> not $ p `inRegion` r && p `inRegion` outside r

  , QC.testProperty "`inRegion` is preserved by translation"
      \(r, p@(px,py), d@(dx,dy)) ->  p `inRegion` r == (px+dx,py+dy) `inRegion` translate d r

  , QC.testProperty "outside of outside is inside"
      \r p -> p `inRegion` r == p `inRegion` outside (outside r)
  ]

geoServerShallowHedgehogProps :: TestTree
geoServerShallowHedgehogProps = testGroup "hedgehog tests"
  [ testProperty "" $ property do
      r <- forAll genRegion
      p <- forAll genPoint
      assert (p `inRegion` r || p `inRegion` outside r)

  , testProperty "" $ property do
      r <- forAll genRegion
      p <- forAll genPoint
      assert (not $ p `inRegion` r && p `inRegion` outside r)

  , testProperty "`inRegion` is preserved by translation" $ property do
      r <- forAll genRegion
      p@(px,py) <- forAll genPoint
      d@(dx,dy) <- forAll genPoint
      p `inRegion` r === (px+dx,py+dy) `inRegion` translate d r

  , testProperty "outside of outside is inside" $ property do
      r <- forAll genRegion
      p <- forAll genPoint
      p `inRegion` r === p `inRegion` outside (outside r)
  ]

-- QC requires generated input to be an instance of Show
instance Show Region where
  show _ = "<region>"

instance QC.Arbitrary Region where
  arbitrary =
    QC.frequency [ (5, circle <$> QC.arbitrary)
                 , (5, square <$> QC.arbitrary)
                 , (2, liftA2 (/\) QC.arbitrary QC.arbitrary)
                 , (2, outside <$> QC.arbitrary)
                 , (2, translate <$> QC.arbitrary <*> QC.arbitrary)
                 , (1, annulus <$> QC.arbitrary <*> QC.arbitrary)
                 ]

genRegion :: Gen Region
genRegion =
  Gen.frequency [ (5, circle <$> Gen.double (Range.linearFrac 0 100))
                , (5, square <$> Gen.double (Range.linearFrac 0 100))
                , (2, liftA2 (/\) genRegion genRegion)
                , (2, outside <$> genRegion)
                , (2, translate <$> genPoint <*> genRegion)
                , (1, annulus <$> Gen.double (Range.linearFrac 0 100) <*> Gen.double (Range.linearFrac 0 100))
                ]

genPoint :: Gen Point
genPoint = (,) <$> Gen.double (Range.linearFrac 0 100) <*> Gen.double (Range.linearFrac 0 100)
