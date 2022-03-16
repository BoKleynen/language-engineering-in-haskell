module Lambda where

import Miniparsec


data Expr
  = App Expr Expr           -- application
  | Lam String Expr         -- lambda abstraction
  | Let String Expr Expr    -- local definition
  | Var String              -- variable

expr :: Parser Expr
expr = atom `chainl1` pure App

atom :: Parser Expr
atom = lam +++ local +++ var +++ paren

lam :: Parser Expr
lam = do
  symbol "\\"
  x <- variable
  symbol "->"
  e <- expr
  return (Lam x e)

local :: Parser Expr
local = do
  symbol "let"
  x <- variable
  symbol "="
  e <- expr
  symbol "in"
  e' <- expr
  return (Let x e e')

var :: Parser Expr
var = Var <$> variable

paren :: Parser Expr
paren = bracket (symbol "(") expr (symbol ")")

variable :: Parser String
variable = identifier ["let", "in"]
