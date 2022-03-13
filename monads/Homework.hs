module Homework where

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.State

data Term
  = Con Int
  | Div Term Term
  deriving Show

data EvalException
  = DivideByZero
  deriving Show

type M = WriterT String (StateT Int (Except EvalException))

eval :: Term -> M Int
eval term@(Con n) = do
  tell (line term n)
  return n
eval term@(Div t u) = do
  a <- eval t
  b <- eval u
  when (b == 0) (throwError DivideByZero)
  tick
  let result = a `div` b
  tell (line term result)
  return result

tick :: M ()
tick = state (\s -> ((), s+1))

line :: Term -> Int -> String
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n"


runM :: M Int -> Either EvalException ((Int, String), Int)
runM m = runExcept $ flip runStateT 0 $ runWriterT m

printResult :: M Int -> IO ()
printResult m = case runM m of
  Left e -> putStrLn ("Error: " ++ show e)
  Right ((res, log), s) -> do
    putStrLn ("Result = " ++ show res)
    putStrLn (show s ++ " divisions")
    putStrLn ("Log: \n" ++ log)
