{-# LANGUAGE TemplateHaskell #-}

import Lib

-- f x = $(power 3) x

-- g x = $( power' 3 [| x |] )

h :: Int -> Int
h x = $( power'' 9 [| x |] )

-- i x = $(fastpower1 5) x

-- j x = $(fastpower2 5 [| x |])

j' :: Num a => a -> a
j' x = $(fastpower2 9 [| x |])

k :: Num a => a -> a
k x = $(fastpower3 9 [| x |])
