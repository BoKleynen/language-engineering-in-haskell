{-# LANGUAGE GADTs #-}

module Expr where

import Control.Applicative
import Control.Monad (MonadPlus(mzero))

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

match' :: MonadPlus m => RE a  -> String -> m (a, String)
match' (Char c) (d:ds) | c == d     = pure (d,ds)
                       | otherwise  = mzero
match' (Char _) []                  = mzero
match' (Alt re1 re2) s = match' re1 s <|> match' re2 s
match' (Cat f re1 re2) s =
  do
    (x, s') <- match' re1 s
    (y, s'') <- match' re2 s'
    return (f x y, s'')
match' (Eps x) s = pure (x, s)
match' Empty _ = mzero
match' (Star re) s = match' (Alt (Cat (:) re (Star re)) (Eps mzero)) s

match :: RE a -> String -> [a]
match re s = map fst . filter (\(_, s') -> null s') $ match' re s

matchMaybe :: RE a -> String -> Maybe a
matchMaybe re s = do
  (x, s') <- match' re s
  if null s'
    then Just x
    else Nothing
