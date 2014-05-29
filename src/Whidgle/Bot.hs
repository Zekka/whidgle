{-
 - Whidgle.Bot
 -
 - Describes Whidgle's actual behavior.
 -}
module Whidgle.Bot
( bot
) where

import Control.Applicative
import Control.Lens
import Control.Monad.Loops
import Control.Monad.State

import Data.Maybe
import Data.Monoid

import System.Timeout

import Whidgle.Comparisons
import Whidgle.Hoisting
import Whidgle.Pathfinding
import Whidgle.POIs
import Whidgle.Types

-- Whidgle's main loop.
bot :: Bot
bot = Bot
  { initialize =
      use activityViewUrl >>= liftIO . print

  , turn =
      -- try to run the main body of the bot, but time out if it takes too long and result in Stay
      fromMaybe Stay <$> ioHoistWhidgle (timeout timeLimit) (botMain undefined)
  }
  where
  -- time limit: .5sec
  timeLimit = 500000
  -- parameter is used to maintain laziness
  -- (yes, this is a hack)
  botMain _ = do
    (Just (POI target _)) <-
      -- find the best POI first by score and then by distnace
      maximumByM
        (  comparingM scorePOI
        <> comparingM (fmap negate . distPOI)
        ) =<< getPOIs

    -- if the route worked, follow it -- otherwise, stay
    maybe (return Stay) follow =<< approach target
