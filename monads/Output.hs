{-# LANGUAGE InstanceSigs #-}

module Output where

import Control.Monad (liftM2)
import Control.Monad.Writer
import Data.Monoid


type Output = String

type M a = (Output, a)

data Term
  = Con Int
  | Div Term Term
  deriving Show

eval :: Term -> M Int
eval (Con a)   = (line (Con a) a, a)
eval (Div t u) = (x ++ y ++ line (Div t u) (a `div` b), a `div` b)
  where
    (x,a) = eval t
    (y,b) = eval u

line :: Term -> Int -> Output
line t a = "eval (" ++ show t ++ ") <=" ++ show a ++ "\n"


newtype OutputM o a = Output (o, a)

instance Functor (OutputM o) where
  fmap :: (a -> b) -> OutputM o a -> OutputM o b
  fmap g (Output (o, a)) = Output (o, g a)

instance Monoid o => Applicative (OutputM o) where
  pure :: a -> OutputM o a
  pure a = Output (mempty, a)

  (<*>) :: OutputM o (a -> b) -> OutputM o a -> OutputM o b
  (Output (o1, g)) <*> (Output (o2, x)) = Output (o1 <> o2, g x)

instance Monoid o => Monad (OutputM o) where
  return :: a -> OutputM o a
  return = pure

  (>>=) :: OutputM o a -> (a -> OutputM o b) -> OutputM o b
  (Output (o, x)) >>= g = Output (o <> o', b)
    where
      Output (o', b) = g x

out :: Output -> OutputM Output ()
out x = Output (x, ())

evalM :: Term -> OutputM Output Int
evalM (Con a)   = out (line (Con a) a) >> return a
evalM (Div t u) = do
  q <- liftM2 div (evalM t) (evalM u)
  out (line (Div t u) q) >> return q

fac :: Int -> Writer (Sum Int) Int
fac 0 = return 1
fac n = do
  tell 1
  x <- fac (n-1)
  return (n*x)
