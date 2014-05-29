{-
 - Whidgle.Temporal
 -
 - Describes how Whidgle reasons about utility (gold) over time.
 -}
module Whidgle.Temporal
( temporal
, average
, overTime
, spike, constantly
) where

import Control.Monad.Reader

import Whidgle.Types

-- Converts a function from time to value to a Temporal.
temporal :: (Int -> a) -> Temporal a
temporal f = do
  v <- ask
  return (f v)

-- Finds the value of a Temporal at a given moment.
-- Use like this: temporalThing `atMoment` 1.
atMoment :: Temporal a -> Int -> a
atMoment = runReader

-- Finds the average value of a Temporal over time.
average :: (Num a, Fractional a) => Int -> Int -> Temporal a -> a
average current duration t
  = overTime current duration t / fromIntegral duration

-- Finds the total value of a Temporal over time.
overTime :: Num a => Int -> Int -> Temporal a -> a
overTime current duration t
  = sum (map (t `atMoment`) [current..current + duration])

-- Generates a Temporal whose value is fixed except at a specific time.
spike :: Int -> a -> a -> Temporal a
spike t def val = temporal $
  \t' -> if t == t' then val else def

-- Generates a Temporal whose value is constant.
constantly :: a -> Temporal a
constantly = return
