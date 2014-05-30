{-
 - Whidgle.CompositeMap
 -
 - Describes operations on the type representing the pathfinding knowledge of all heroes.
 -}
module Whidgle.CompositeMap
( generateMaps
) where

import Control.Parallel.Strategies

import qualified Data.Map as M

import Whidgle.Pathfinding
import Whidgle.Types

-- Generates a RouteMap for every hero, caching its contents.
generateMaps :: Activity -> [Hero] -> CompositeMap
generateMaps act heroes = M.fromList $ parMap rpar makeRoute heroes
  where
  makeRoute h@(Hero {_heroId = hId, _heroPos = hPos}) =
    (hId, mapFrom act h hPos)
