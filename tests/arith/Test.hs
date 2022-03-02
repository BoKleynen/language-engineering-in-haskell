module Main where

import Hedgehog ( (===), forAll, property, Gen, Property )
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Test.QuickCheck as Quick
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Arith


main :: IO ()
main = defaultMain (testGroup "All Tests" [ testGroup "Properties" props ])

genExp :: Gen Exp
genExp =
  Gen.frequency [ (3, Const <$> Gen.integral (Range.linear minBound maxBound))
                , (1, Add <$> genExp <*> genExp)
                , (1, Mul <$> genExp <*> genExp)
                , (1, Sub <$> genExp <*> genExp)
                ]

props =
  [ testProperty "eval and run . compile agree" prop_eval
  ]

prop_eval :: Property
prop_eval = property $ do
  exp <- forAll genExp
  [eval exp] === run (compile exp) []


-- instance Quick.Arbitrary Exp where
--   arbitrary =
--     Quick.frequency [ (3, Const <$> Quick.arbitrary)
--               , (1, Add <$> Quick.arbitrary <*> Quick.arbitrary)
--               , (1, Mul <$> Quick.arbitrary <*> Quick.arbitrary)
--               , (1, Sub <$> Quick.arbitrary <*> Quick.arbitrary)
--               ]

  -- shrink (Const n)   = [ Const n' | n' <- shrink n ]
  -- shrink (Add e1 e2) = [ Add e1' e2 | e1' <- shrink e1] ++  [ Add e1 e2' | e2' <- shrink e2]
  -- shrink (Mul e1 e2) = [ Mul e1' e2 | e1' <- shrink e1] ++  [ Mul e1 e2' | e2' <- shrink e2]
  -- shrink (Sub e1 e2) = [ Sub e1' e2 | e1' <- shrink e1] ++  [ Sub e1 e2' | e2' <- shrink e2]

-- example = Sub (Add (Const 2) (Const 3)) (Const 4)

-- unittest = run (compile example) [] == [1]

-- expproperty e = [eval e] == run (compile e) []
