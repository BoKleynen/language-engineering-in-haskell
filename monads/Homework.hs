module Homework where

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.State

data Term
  = Con Int
  | Div Term Term
  deriving Show

type M a = WriterT String (StateT Int (Except String)) a

eval :: Term -> M Int
eval term @ (Con n) = do
  tell (line term n)
  return n
eval term @ (Div t u) = do
  a <- eval t
  b <- eval u
  when (b == 0) (throwError "divide by zero")
  state (\s -> ((), s+1))
  let result = a `div` b
  tell (line term result)
  return result


line :: Term -> Int -> String
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n"


runM :: M Int -> Either String ((Int, String), Int)
runM m = runExcept $ runStateT (runWriterT m) 0

printResult :: M Int -> IO ()
printResult m = case runM m of
  Left e -> putStrLn ("Error: " ++ e)
  Right ((res, log), s) -> do
    putStrLn ("Result = " ++ show res)
    putStrLn (show s ++ " divisions")
    putStrLn ("Log: \n" ++ log)
