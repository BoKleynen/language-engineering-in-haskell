{-# LANGUAGE InstanceSigs #-}

module State where
import Control.Monad (liftM2)

type State = Int

type M a = State -> (a, State)

data Term
  = Con Int
  | Div Term Term

eval :: Term -> M Int
eval (Con a) x = (a, x)
eval (Div t u) x =
  let (a,y) = eval t x
      (b,z) = eval u y        -- Passing in the state everywhere is hard
  in  (a `div` b, z + 1)


newtype StateM s a = State (s -> (a, s))

runState :: s -> StateM s a -> (a,s)
runState s (State m) = m s

instance Functor (StateM s) where
  fmap :: (a -> b) -> StateM s a -> StateM s b
  fmap g x = x >>= \a -> return (g a)
  -- fmap g (State x) = State (
  --   \s0 -> let (x', s1) = x s0
  --          in (g x', s1)
  --   )

instance Applicative (StateM s) where
  pure :: a -> StateM s a
  pure x = State (\s -> (x, s))

  (<*>) :: StateM s (a -> b) -> StateM s a -> StateM s b
  g <*> x = g >>= \f -> x >>= \y -> return (f y)
  -- (State g) <*> (State x) = State (
  --   \s0 -> let (g', s1) = g s0
  --              (x', s2) = x s1
  --         in (g' x', s2)
  --   )

instance Monad (StateM s) where
  return :: a -> StateM s a
  return = pure

  (>>=) :: StateM s a -> (a -> StateM s b) -> StateM s b
  (State x) >>= g = State (
    \s0 -> let (x', s1) = x s0
               State y = g x'
           in y s1
    )

get :: StateM s s
get = State (\s -> (s, s))

put :: s -> StateM s ()
put s = State (const ((), s))

tick :: StateM Int ()
tick = State (\s -> ((), s + 1))

evalM :: Term -> StateM Int Int
evalM (Con a) = return a
evalM (Div t u) = tick >> liftM2 div (evalM t) (evalM u)

numbers :: [a] -> StateM Int [(Int,a)]
numbers []     = pure []
numbers (x:xs) = do
  n <- get
  put (n+1)
  ys <- numbers xs
  return ((n,x) : ys)
