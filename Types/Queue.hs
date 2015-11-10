{-# OPTIONS_GHC -O2 -Wall #-}
-------------------------------------------------------------------------------
-- Queue.hs: Queue type and critical operations
-- 
-- Author: Jeremy Nuttall
-------------------------------------------------------------------------------
module Types.Queue (
      Queue
    , singleton
    , empty
    , listApp
    , pop) where

-- Create a queue from two lists using weak head normal form
data Queue a = Queue ![a] ![a]

-- Construct a queue with a single item
singleton :: a -> Queue a
singleton y = Queue [] [y]
{-# INLINE singleton #-}

-- Test whether the queue is empty
empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _             = False
{-# INLINE empty #-}

-- Append a list of items to the queue
listApp :: [a] -> Queue a -> Queue a 
listApp []     queue = queue
listApp (v:vs) queue = listApp vs (append v queue)
  where
    append y (Queue xs ys) = Queue xs (y:ys)

-- Pop the first item from the queue
pop :: Queue a -> (a, Queue a)
pop (Queue [] [])     = error "Queue.hs: Trying to pop from empty queue."
pop (Queue [] ys)     = pop (Queue (reverse ys) [])
pop (Queue (x:xs) ys) = (x, Queue xs ys)
