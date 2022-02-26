module Basic where

data Term
  = Con Int
  | Div Term Term

eval :: Term -> Int
eval (Con a)   = a
eval (Div t u) = eval t `div` eval u
