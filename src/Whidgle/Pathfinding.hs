{-
 - Whidgle.Pathfinding
 -
 - Describes how Whidgle finds its way around the map.
 -}
module Whidgle.Pathfinding
( mapFrom
, distance, manhattan
, follow
, approach
) where

import Control.Applicative
import Control.Lens
import Control.Monad.State

import Data.Function.Memoize

-- import Debug.Trace

import Whidgle.Board
import Whidgle.Dijkstra
import Whidgle.Types

-- Finds what cells are adjacent/accessible from the given position.
adjacent :: Pos -> Board -> Pos -> [Pos]
adjacent start b p@(Pos x y) =
  if p == start || isPassable (tileAt b p)
    -- if we're either (a) where we started from or (b) at a passable tile, then
    -- try the nearby positions
    then filter (isAccessible . tileAt b) options
    else [] -- you can't go anywhere useful after you've gotten there
  where
  options =
    [ Pos (x - 1) y
    , Pos (x + 1) y
    , Pos x (y - 1)
    , Pos x (y + 1)
    ]

-- Manhattan distance between Poses.
manhattan :: Pos -> Pos -> Int
manhattan (Pos x1 y1) (Pos x2 y2) = abs (y2 - y1) + abs (x2 - x1)

-- Generates the RouteMap associated with a Pos on the current Board.
mapFrom :: Board -> Pos -> RouteMap
mapFrom b start =
  let dijMap = buildDij (adjacent start b) manhattan start in
  memoize $ \goal -> Route <$>
    -- aStar (adjacent start b) manhattan (manhattan goal) (== goal) start
    pathDij goal dijMap

-- Determines the length of a Route.
distance :: Route -> Int
distance (Route ps) = length ps

-- Follows a Route, determining the next Dir to move in.
follow :: Route -> Whidgle Dir
follow (Route []) = return Stay
follow (Route (Pos nextX nextY:_)) = do
  (Pos x y) <- use $ activityHero.heroPos
  return $ case (nextX - x, nextY - y) of
    (-1, 0) -> North
    ( 1, 0) -> South
    (0, -1) -> West
    (0,  1) -> East
    _       -> Stay

-- Determines the best route to a given location from the current position.
approach :: Pos -> Whidgle (Maybe Route)
approach location = fmap ($ location) (gets fetchActivityOurMap)
