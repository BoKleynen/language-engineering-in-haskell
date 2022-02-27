module Exceptions where

type Exception = String

data M a
  = Raise Exception
  | Return a

data Term
  = Con Int
  | Div Term Term

eval :: Term -> M Int
eval (Con a)   = Return a
eval (Div t u) = case eval t of
  Raise e -> Raise e                -- Checking these values becomes quite tedious and noisy
  Return a -> case eval u of
    Raise e -> Raise e
    Return b ->
      if b == 0
        then Raise "divide by zero"
        else Return (a `div` b)

instance Functor M where
  fmap _ (Raise e)  = Raise e
  fmap g (Return x) = Return (g x)

instance Applicative M where
  pure = Return

  (Return g) <*> (Return x) = Return (g x)
  (Raise e) <*> _ = Raise e
  _ <*> (Raise e) = Raise e

instance Monad M where
  return = pure

  (Return x) >>= g = g x
  (Raise e) >>= _ = Raise e
