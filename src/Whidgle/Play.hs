{-
 - Whidgle.Play
 -
 - Taken from the starter -- explains how to control Whidgle with the Vindinium
 - API. This is all glue code.
 -}
module Whidgle.Play
( playTraining
, playArena
) where

import Control.Lens
import Control.Monad.State

import Whidgle.Api
import Whidgle.Types

-- Starts to run training matches with a bot,
-- possibly with a fixed turn count or board.
playTraining :: Maybe Int -> Maybe Board -> Bot -> Whidgle Activity
playTraining mt mb = playMain (startTraining mt mb)

-- Plays an arena match.
playArena :: Bot -> Whidgle Activity
playArena = playMain startArena

-- Actually handles playing after we've started.
playMain :: Whidgle Activity -> Bot -> Whidgle Activity
playMain start b = do
  a <- start
  put a
  initialize b
  playLoop b a

-- Used internally -- a loop to advance to the next turn at each opportunity.
playLoop :: Bot -> Activity -> Whidgle Activity
playLoop bot ac =
  if ac^.activityGame.gameFinished
    then return ac
    else do
      put ac
      newActivity <- turn bot >>= move ac
      playLoop bot newActivity
