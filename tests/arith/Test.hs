module Main where

import Hedgehog ( (===), forAll, property, Gen, Property )
import Test.Tasty

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty.Hedgehog as H
import qualified Test.Tasty.QuickCheck as QC

import Arith


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ hedgehogProps
  , qcProps
  ]

hedgehogProps :: TestTree
hedgehogProps = testGroup "hedgehog tests"
  [ H.testProperty "eval and run . compile agree" prop_eval
  ]


qcProps :: TestTree
qcProps = testGroup "quickcheck tests"
  [ QC.testProperty "TestName" $
      \e -> [eval e] == run (compile e) []
  ]

genExp :: Gen Exp
genExp =
  Gen.frequency [ (3, Const <$> Gen.integral (Range.linear minBound maxBound))
                , (1, Add <$> genExp <*> genExp)
                , (1, Mul <$> genExp <*> genExp)
                , (1, Sub <$> genExp <*> genExp)
                ]

prop_eval :: Property
prop_eval = property $ do
  exp <- forAll genExp
  [eval exp] === run (compile exp) []


instance QC.Arbitrary Exp where
  arbitrary =
    QC.frequency [ (3, Const <$> QC.arbitrary)
                 , (1, Add <$> QC.arbitrary <*> QC.arbitrary)
                 , (1, Mul <$> QC.arbitrary <*> QC.arbitrary)
                 , (1, Sub <$> QC.arbitrary <*> QC.arbitrary)
                 ]

  shrink (Const n)   = [ Const n' | n' <- QC.shrink n ]
  shrink (Add e1 e2) = [ Add e1' e2 | e1' <- QC.shrink e1] ++  [ Add e1 e2' | e2' <- QC.shrink e2]
  shrink (Mul e1 e2) = [ Mul e1' e2 | e1' <- QC.shrink e1] ++  [ Mul e1 e2' | e2' <- QC.shrink e2]
  shrink (Sub e1 e2) = [ Sub e1' e2 | e1' <- QC.shrink e1] ++  [ Sub e1 e2' | e2' <- QC.shrink e2]
