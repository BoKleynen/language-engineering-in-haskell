{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Language.Haskell.TH

app1 :: Int -> Int
app1 = power0 7

app1' :: Num a => a -> a
app1' x = x * x * x

power0 :: Int -> Int -> Int
power0 0 _ = 1
power0 n x = x * power0 (n-1) x

power3 :: Int -> TCode Int -> TCode Int
power3 0 _ = [|| 1 ||]
power3 n x = go n
  where
    go 1 = [|| $$x ||]
    go k = [|| $$x * $$( go (k-1)) ||]

power_1 :: Int -> String
power_1 0 = "(\\ x -> 1)"
power_1 n = "(\\ x -> x * " ++ power_1 (n - 1) ++ " x)"

type Code = Q Exp

power :: Int -> Code
power 0 = [| const 1 |]
power n = [| \ x -> x * $( power (n-1) ) x |]

power' :: Int -> Code -> Code
power' 0 _ = [| 1 |]
power' n x = [| $x * $( power' (n-1) x) |]

power'' :: Int -> Q Exp -> ExpQ
power'' 0 _ = [| 1 |]
power'' n x = go n
  where
    go 1 = [| $x |]
    go k = [| $x * $( go (k-1)) |]

type TCode a = Q (TExp a)

-- x^(2 * m) = (x^2)^m

fastpower0 :: (Num a2, Integral a1) => a1 -> a2 -> a2
fastpower0 0 = const 1
fastpower0 1 = id
fastpower0 n | even n    =  \ x -> fastpower0 (n `div` 2) (x * x)
             | otherwise =  \ x -> x * fastpower0 (n - 1) x

fastpower1 :: Integral a => a -> ExpQ
fastpower1 0 = [| const 1 |]
fastpower1 1 = [| id |]
fastpower1 n | even n    =  [| \ x -> $(fastpower1 (n `div` 2)) (x * x) |]
             | otherwise =  [| \ x -> x * $(fastpower1 (n - 1)) x |]

fastpower2 :: Int -> ExpQ -> ExpQ
fastpower2 0 _ = [| 1 |]
fastpower2 1 x = x
fastpower2 n x | even n    =  fastpower2 (n `div` 2) [| $x * $x |]
               | otherwise =  [| $(x) * $(fastpower2 (n - 1) x) |]

fastpower3 :: Int -> ExpQ -> ExpQ
fastpower3 0 _ = [| 1 |]
fastpower3 1 x = x
fastpower3 n x | even n    =  [| let y = $x * $x in $(fastpower3 (n `div` 2) [| y |]) |]
               | otherwise =  [| $(x) * $(fastpower3 (n - 1) x) |]
