{-# LANGUAGE InstanceSigs #-}

module Output where
import Control.Monad (liftM2)

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


newtype OutputM o a = MkOutput (o, a)

instance Functor (OutputM o) where
  fmap :: (a -> b) -> OutputM o a -> OutputM o b
  fmap g (MkOutput (o, a)) = MkOutput (o, g a)

instance Monoid o => Applicative (OutputM o) where
  pure :: a -> OutputM o a
  pure a = MkOutput (mempty, a)

  (<*>) :: OutputM o (a -> b) -> OutputM o a -> OutputM o b
  (MkOutput (o1, g)) <*> (MkOutput (o2, x)) = MkOutput (o1 <> o2, g x)

instance Monoid o => Monad (OutputM o) where
  return :: a -> OutputM o a
  return = pure

  (>>=) :: OutputM o a -> (a -> OutputM o b) -> OutputM o b
  (MkOutput (o, x)) >>= g = MkOutput (o <> o', b)
    where
      MkOutput (o', b) = g x

out :: Output -> OutputM Output ()
out x = MkOutput (x, ())

evalM :: Term -> OutputM Output Int
evalM (Con a)   = out (line (Con a) a) >> return a
evalM (Div t u) = do
  q <- liftM2 div (evalM t) (evalM u)
  out (line (Div t u) q) >> return q
