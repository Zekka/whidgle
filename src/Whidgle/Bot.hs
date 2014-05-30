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

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (unpack)

import System.Timeout

import Whidgle.Comparisons
import Whidgle.Hoisting
import Whidgle.Pathfinding
import Whidgle.POIs
import Whidgle.Rules
import Whidgle.Types

-- Whidgle's main loop.
bot :: Bot
bot = Bot
  { initialize = do
      use (session.activityViewUrl) >>= liftIO . putStrLn . unpack
      internal.= Internal
        { _avoidanceRatios = M.empty
        , _lastDistances = M.empty
        , _target = Nothing
        }

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
    (Just (POI target meta)) <-
      -- find the best POI first by score and then by distnace
      maximumByM
        (  comparingM scorePOI
        <> comparingM (fmap negate . distPOI)
        ) =<< getPOIs

    route <- approach target
    process meta route

    -- if the route worked, follow it -- otherwise, stay
    maybe (return Stay) follow route

  -- update internal state based on selected POI
  process (HasHero (Hero {_heroId = hId})) route = do
    let expandDist = fromMaybe overwhelming
    let len = expandDist (fmap distance route)
    -- check if we were avoided
    tar <- use (internal.target)
    flip (maybe (return ())) tar $ \t -> do
      lastDist <- use (internal.lastDistances.at hId.to expandDist)
      internal.avoidanceRatios.at t %=
        let
        modifier = Just .
          if (t == hId && len >= lastDist)
            then \(x, y) -> (x + 1, y + 1) -- it's further or as far -- one more avoidance
            else \(x, y) -> (x, y + 1) -- it's the same -- one less avoidance
        in maybe (modifier (0, 0)) modifier

    -- update internals
    internal.target .= Just hId
    internal.lastDistances.at hId .= fmap distance route

  process _ _ = do
    -- lose target
    tar <- use $ internal.target
    flip (maybe (return ())) tar $ \t ->
      internal.lastDistances.at t .= Nothing
    internal.target .= Nothing
