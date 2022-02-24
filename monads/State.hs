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


newtype StateM a = MkState (State -> (a, State))

instance Functor StateM where
    fmap :: (a -> b) -> StateM a -> StateM b
    fmap g (MkState x) = MkState (
        \s -> let (x', s') = x s
              in (g x', s')
        )

instance Applicative StateM where
    pure :: a -> StateM a
    pure x = MkState (\s -> (x, s))

    (<*>) :: StateM (a -> b) -> StateM a -> StateM b
    (MkState g) <*> (MkState x) = MkState (
        \s -> let (g', s') = g s
                  (x', s'') = x s'
              in (g' x', s'')
        )

instance Monad StateM where
    return :: a -> StateM a
    return = pure

    (>>=) :: StateM a -> (a -> StateM b) -> StateM b
    (MkState x) >>= g = MkState (
        \s -> let (x', s') = x s
                  MkState y = g x'
              in y s'
        )

tick :: StateM ()
tick = MkState (\s -> ((), s + 1))

evalM :: Term -> StateM Int
evalM (Con a) = return a
evalM (Div t u) = tick >> liftM2 div (evalM t) (evalM u)
