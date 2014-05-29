{-
 - Whidgle.Temporal
 -
 - Describes how Whidgle reasons about utility (gold) over time.
 -}
module Whidgle.Temporal
( Temporal, negative, timespan, constant, both, spike
, atMoment, average, overTime
) where

data Temporal a
  = Negative (Temporal a)
  | Timespan a Int Int (Temporal a)
  | Both (Temporal a) (Temporal a)
  | Spike a Int a
  | Constant a

-- negates another
-- negative temporal :: Temporal a
negative :: Num a => Temporal a -> Temporal a
negative (Negative x) = x
negative x = Negative x

-- a specific Temporal within a certain range: otherwise a constant value
-- timespan constant begin end temporal :: Temporal a
timespan :: Num a => a -> Int -> Int -> Temporal a -> Temporal a
timespan = Timespan

-- a constant value
-- constant value :: Temporal a
constant :: Num a => a -> Temporal a
constant = Constant

-- a constant value at a specific moment and otherwise a different one
-- spike constant moment spikeValue :: Temporal a
spike :: Num a => a -> Int -> a -> Temporal a
spike = Spike

-- the sum of two other temporals
-- both t1 t2 :: Temporal a
both :: Num a => Temporal a -> Temporal a -> Temporal a
both = Both

-- Finds the value of a Temporal at a given moment.
-- Use like this: temporalThing `atMoment` 1.
atMoment :: Num a => Temporal a -> Int -> a
atMoment t x = overTime x (x + 1) t

-- Finds the average value of a Temporal over time.
average :: (Num a, Fractional a) => Int -> Int -> Temporal a -> a
average current duration t
  = overTime current duration t / fromIntegral duration

-- Finds the total value of a Temporal over time.
overTime :: Num a => Int -> Int -> Temporal a -> a
overTime c d (Negative t) = negate (overTime c d t)
overTime current duration (Timespan def mn mx t) =
  let
  timeMin = current
  timeMax = current + duration
  tsMin = max timeMin mn
  tsMax = min timeMax mx
  -- time spent in the constant
  timeConstant = tsMin - timeMin + timeMax - tsMax
  in
  overTime tsMin (tsMax - tsMin) t + fromIntegral timeConstant * def
overTime c d (Spike def moment spikeVal)
  | moment >= c && moment < c + d = spikeVal + fromIntegral (d - 1) * def
  | otherwise = fromIntegral d * def
overTime c d (Both t1 t2) = overTime c d t1 + overTime c d t2

overTime _ duration (Constant x) = fromIntegral duration * x
