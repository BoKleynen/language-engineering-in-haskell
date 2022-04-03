module Arith where

data Exp
  = Const Int
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  deriving (Show, Eq)

eval :: Exp -> Int
eval (Const i)   = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog   = [Inst]
type Stack  = [Int]

runtimeError :: Stack
runtimeError = error "Runtime error."

execute :: Inst -> Stack -> Stack
execute (IPush i) s       = i : s
execute IAdd      (x:y:s) = y + x : s
execute ISub      (x:y:s) = y - x : s
execute IMul      (x:y:s) = y * x : s
execute _         _       = runtimeError

run :: Prog -> Stack -> Stack
run xs s = foldl (flip execute) s xs

compile :: Exp -> Prog
compile (Const i) = [IPush i]
compile (Add a b) = compile a ++ compile b ++ [IAdd]
compile (Sub a b) = compile a ++ compile b ++ [ISub]
compile (Mul a b) = compile a ++ compile b ++ [IMul]
