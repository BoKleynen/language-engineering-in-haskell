module Main where

import Hedgehog ( (===), forAll, property, Gen, Property )
import Test.Tasty

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty.Hedgehog as H
import qualified Test.QuickCheck as Quick
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


instance Quick.Arbitrary Exp where
  arbitrary =
    Quick.frequency [ (3, Const <$> Quick.arbitrary)
                    , (1, Add <$> Quick.arbitrary <*> Quick.arbitrary)
                    , (1, Mul <$> Quick.arbitrary <*> Quick.arbitrary)
                    , (1, Sub <$> Quick.arbitrary <*> Quick.arbitrary)
                    ]

  shrink (Const n)   = [ Const n' | n' <- Quick.shrink n ]
  shrink (Add e1 e2) = [ Add e1' e2 | e1' <- Quick.shrink e1] ++  [ Add e1 e2' | e2' <- Quick.shrink e2]
  shrink (Mul e1 e2) = [ Mul e1' e2 | e1' <- Quick.shrink e1] ++  [ Mul e1 e2' | e2' <- Quick.shrink e2]
  shrink (Sub e1 e2) = [ Sub e1' e2 | e1' <- Quick.shrink e1] ++  [ Sub e1 e2' | e2' <- Quick.shrink e2]
