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

match' :: RE a  -> String -> Maybe (a, String)
match' (Char c) (d:ds) | c == d     = Just (d,ds)
                       | otherwise  = Nothing
match' (Char c) []                  = Nothing
match' (Alt re1 re2) s = match' re1 s <|> match' re2 s
match' (Cat f re1 re2) s = do
  (x, s') <-  match' re1 s
  (y, s'') <- match' re2 s'
  return (f x y, s'')
match' (Eps x) s = Just (x, s)
match' Empty _ = Nothing
match' (Star re) s = match' (Alt (Cat (:) re (Star re)) (Eps [])) s

match :: RE a -> String -> Maybe a
match re s = do
  (x, s') <- match' re s
  if null s'
    then Just x
    else Nothing
