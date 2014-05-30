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
import Whidgle.Util

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
      -- timeouts fuck with State, so they're disabled for now
      -- fromMaybe Stay <$> ioHoistWhidgle (timeout timeLimit) (botMain undefined)
      botMain undefined
  }
  where
  -- time limit: .5sec
  timeLimit = 500000
  -- parameter is used to maintain laziness
  -- (yes, this is a hack)
  botMain _ = do
    turnNo <- use $ session.activityGame.gameTurn
    lputs $ "-- turn " ++ show (turnNo `div` 4) ++ " (" ++ show (turnNo `mod` 4) ++ ") --"

    (Just p@(POI target meta)) <-
      -- find the best POI first by score and then by distnace
      maximumByM
        (  comparingM scorePOI
        <> comparingM (fmap negate . distPOI)
        ) =<< getPOIs

    sc <- scorePOI p
    if True {- sc > 0 -} then do
      lputs $ "targeting " ++ showPOI meta ++ " (" ++ show target ++ ")"
      route <- approach target
      process meta route

      -- if the route worked, follow it -- otherwise, stay
      result <- maybe (return Stay) follow route
      return result
    else do
      lputs $ "no target; staying"
      return Stay -- we don't need to do things that just get us killed

  showPOI (HasTavern) = "a tavern"
  showPOI (HasMine) = "a mine"
  showPOI (HasHero (Hero { _heroName = hn})) = unpack hn

  -- update internal state based on selected POI
  process (HasHero (Hero {_heroId = hId, _heroName = hn})) route = do
    let expandDist = fromMaybe overwhelming
    let len = expandDist (fmap distance route)
    -- check if we were avoided
    tar <- use (internal.target)
    flip (maybe (lputs "no previous player target -- skipping target update logic")) tar $ \t -> do
      lastDist <- use $ internal.lastDistances.at hId.to expandDist
      let itHasAvoidedMe = t == hId && len >= lastDist
      lputs $ "it has" ++ (if itHasAvoidedMe then " not " else " ") ++ "avoided me"

      internal.avoidanceRatios.at t %=
        let
        modifier = Just .
          if itHasAvoidedMe
            then \(x, y) -> (x + 1, y + 1) -- it's further or as far -- one more avoidance
            else \(x, y) -> (x, y + 1) -- it's the same -- one less avoidance
        in maybe (modifier initialAvoidanceRatio) modifier

      ar <- use $ internal.avoidanceRatios.at t
      lputs $ "current avoidance ratio " ++ show ar

    -- update internals
    lputs $ "updating player target (" ++ unpack hn ++ ")"
    internal.target .= Just hId
    internal.lastDistances.at hId .= fmap distance route
    tar2 <- use $ internal.target
    lputs $ "new target id: " ++ show tar2

  process _ _ = do
    -- lose target
    tar <- use $ internal.target
    flip (maybe (return ())) tar $ \t -> do
      lputs "losing player target"
      internal.lastDistances.at t .= Nothing
    internal.target .= Nothing
