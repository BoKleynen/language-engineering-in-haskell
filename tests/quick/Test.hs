import Test.QuickCheck ( orderedList, (==>), forAll, Property, quickCheck )
import Data.List ( insert )


prop_revUnit :: Int -> Bool
prop_revUnit x = reverse [x] == [x]

prop_revApp :: [Int] -> [Int] -> Bool
prop_revApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_revRev :: [Int] -> Bool
prop_revRev xs = reverse (reverse xs) == xs

prop_maxLe :: Int -> Int -> Property
prop_maxLe x y = x <= y ==> max x y == y

prop_insert :: Int -> Property
prop_insert x = forAll orderedList $
    \xs -> ordered (insert x xs)
  where
    ordered :: Ord a => [a] -> Bool
    ordered []       = True
    ordered [_]      = True
    ordered (a:b:xs) = a >= b && ordered (b:xs)

prop_doubleCycle :: [Int] -> Int -> Property
prop_doubleCycle xs n =
  not (null xs) && n >= 0 ==>
    take n (cycle xs) == take n (cycle (xs ++ xs))

main :: IO ()
main = do
  quickCheck prop_revUnit
  quickCheck prop_revApp
  quickCheck prop_revRev
  quickCheck prop_maxLe
  quickCheck prop_insert
  quickCheck prop_doubleCycle
