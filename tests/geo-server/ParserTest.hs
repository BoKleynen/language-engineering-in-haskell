{-# LANGUAGE BlockArguments #-}

module ParserTest (geoServerParserSpecs) where


import Data.Either (isLeft)
import Test.Hspec

import Parser
import GeoServerDeep


geoServerParserSpecs :: Spec
geoServerParserSpecs = describe "Parser" do
  spec_primitiveRegions
  spec_outside
  spec_intersection
  spec_translation
  spec_group

spec_primitiveRegions :: Spec
spec_primitiveRegions =
  describe "primitive regions" do
    it "can parse a circle" $
      parseRegion "(4)" `shouldBe` Right (Circle 4)
    it "can parse a square" $
      parseRegion "[3]" `shouldBe` Right (Square 3)
    it "fails when the radius is missing" $
      parseRegion "()" `shouldSatisfy` isLeft
    it "fails when the side is missing" $
      parseRegion "[]" `shouldSatisfy` isLeft

spec_outside :: Spec
spec_outside =
  describe "outside" do
    it "can parse outside of a primitive region" $
      parseRegion "![4.3]" `shouldBe` Right (Outside (Square 4.3))
    it "allows arbitrary whitespace after !" $
      parseRegion "!   [4.3]" `shouldBe` Right (Outside (Square 4.3))

spec_intersection :: Spec
spec_intersection = do
  describe "intersection" do
    it "can parse the intersection of two primitive regions" $
      parseRegion "[42]/\\(3.14)" `shouldBe` Right (Intersection (Square 42) (Circle 3.14))
    it "allows arbitrary whispace around /\\" $
      parseRegion "[42] /\\ (3.14)" `shouldBe` Right (Intersection (Square 42) (Circle 3.14))
    it "parses -> with higher precedence" $
      parseRegion "[42] -> (1,1) /\\ (3.14)" `shouldBe` Right (Intersection (Translate (1,1) (Square 42)) (Circle 3.14))

spec_translation :: Spec
spec_translation = do
  describe "translation" do
    it "can parse the translation of a primitive region" $
      parseRegion "(2.71)->(3,4)" `shouldBe` Right (Translate (3,4) (Circle 2.71))
    it "allows arbitrary whispace around -> and in the direction" $
      parseRegion "(2.71) -> ( 3 , 4 )" `shouldBe` Right (Translate (3,4) (Circle 2.71))

spec_group :: Spec
spec_group = do
  describe "|...| can be used to group a region" do
    it "can parse a group" $
      parseRegion "|[42] /\\ (3.14)| -> (1,1)" `shouldBe` Right (Translate (1,1) (Intersection (Square 42) (Circle 3.14)))
