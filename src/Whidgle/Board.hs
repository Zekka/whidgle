{-
 - Whidgle.Board
 -
 - Refactored out of the original starter. Describes how the board works.
 -}
module Whidgle.Board
( inBoard
, possibleTileAt, tileAt
, isAccessible, isPassable
, associations, isolate
) where

import Control.Lens

import Data.Maybe

import Whidgle.Types

-- Is a position in the board?
inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
  let s = b^.boardSize
  in x >= 0 && x < s && y >= 0 && y < s

-- What tile, or nothing, is at the given position?
possibleTileAt :: Board -> Pos -> Maybe Tile
possibleTileAt b p@(Pos x y) =
  if inBoard b p
    then Just $ (b^.boardTiles) !! idx
    else Nothing
  where
  -- original math (in starter pack) was wrong for this coordinate system
  idx = x * b^.boardSize + y

-- What tile is at the given position? (WoodTile if full)
tileAt :: Board -> Pos -> Tile
tileAt b p = fromMaybe WoodTile (possibleTileAt b p) -- default to WoodTile

-- Can the hero go to a tile like this?
isAccessible :: Tile -> Bool
isAccessible = (/= WoodTile)

-- Can the hero go anywhere useful after going to a tile like this? (excluding i.e. mines and heroes)
isPassable :: Tile -> Bool
isPassable = (== FreeTile)

-- Returns the board as pairs.
associations :: Board -> [(Pos, Tile)]
associations (Board sz tiles) =
  -- generate a list of locations parallel to the list of tiles
  let locs = [Pos x y | x <- [0..sz - 1], y <- [0..sz - 1]]
  in zip locs tiles

-- `filter` over a board's tiles.
isolate :: (Tile -> Bool) -> Board -> [(Pos, Tile)]
isolate predicate board = filter (predicate . snd) (associations board)
