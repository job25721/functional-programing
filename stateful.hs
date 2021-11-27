{-# LANGUAGE InstanceSigs #-}

import Data.Void

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s ->
    let (x, s') = g s
     in (f x, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State $ \s -> (x, s)
  (<*>) ::
    State s (a -> b) ->
    State s a ->
    State s b
  State h <*> State g = State $ \s ->
    let (f, s') = h s
        (x', s'') = g s'
     in (f x', s'')

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State h >>= f = State $ \s ->
    let (x, s') = h s
        State g = f x
     in g s'

type Stack = [Int]

pop :: State Stack Int
pop = State $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a : xs)

type Queue a = ([a], [a], Int)

mkQueue :: [a] -> State (Queue a) ()
mkQueue xs = State $ \(_, _, _) -> ((), (xs, [], length xs))

size :: State (Queue a) Int
size = State $ \(s1, s2, _) -> (length s1, (s1, s2, length s1))

isEmpty :: State (Queue a) Bool
isEmpty = State $ \(s1, s2, queue) -> (queue == 0, (s1, s2, queue))

empty :: State (Queue a) ()
empty = State $ const ((), ([], [], 0))

enqueue :: a -> State (Queue a) ()
enqueue a = State $ \(s1, _, queue) -> ((), (s1 <> [a], [], queue + 1))

dequeue :: State (Queue a) a
dequeue = State $ \(x : xs, s2, queue) -> (x, (xs, s2 <> [x], queue - 1))

-- dn :: a -> ((a -> Void) -> Void)
-- dn a = (\a -> Void ) -> Void
