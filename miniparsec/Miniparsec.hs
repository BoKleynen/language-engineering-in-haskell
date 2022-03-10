{-# LANGUAGE GADTs #-}

module Miniparsec where

import Control.Applicative

data Expr
  = Con Int
  | Add Expr Expr
  deriving Show

data RE a where
  Char :: Char -> RE Char
  Alt :: RE a -> RE a -> RE a
  Cat :: (b -> c -> a) -> RE b -> RE c -> RE a
  Star :: RE a -> RE [a]
  Eps :: a -> RE a
  Empty :: RE ()

match' :: RE a  -> String -> [(a, String)]
match' (Char c) (d:ds) | c == d     = [(d,ds)]
                       | otherwise  = []
match' (Char c) []                  = []
match' (Alt re1 re2) s = match' re1 s <|> match' re2 s
match' (Cat f re1 re2) s =
  do
    (x, s') <- match' re1 s
    (y, s'') <- match' re2 s'
    return (f x y, s'')
match' (Eps x) s = [(x, s)]
match' Empty _ = []
match' (Star re) s = match' (Alt (Cat (:) re (Star re)) (Eps [])) s

match :: RE a -> String -> [a]
match re s = map fst . filter (\(_, s) -> null s) $ match' re s
