import Test.QuickCheck ( orderedList, (==>), forAll, Property )
import Data.List


prop_revUnit :: Int -> Bool
prop_revUnit x = reverse [x] == [x]

prop_revApp :: [Int] -> [Int] -> Bool
prop_revApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_revRev :: [Int] -> Bool
prop_revRev xs = reverse (reverse xs) == xs

(f === g) x = f x == g x

prop_compAssoc :: (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Int -> Bool
prop_compAssoc f g h = (f . (g . h)) === ((f . g) . h)

prop_maxLe :: Int -> Int -> Property
prop_maxLe x y = x <= y ==> max x y == y

prop_insert :: Int -> [Int] -> Property
prop_insert x xs = forAll orderedList $
    \xs -> ordered (insert x xs)
  where
    ordered :: Ord a => [a] -> Bool
    ordered []       = True
    ordered [_]      = True
    ordered (x:y:xs) = x >= y && ordered (y:xs)

prop_doubleCycle :: [Int] -> Int -> Property
prop_doubleCycle xs n =
  not (null xs) && n >= 0 ==>
    take n (cycle xs) == take n (cycle (xs ++ xs))

main :: IO ()
main = putStrLn "hello world"
