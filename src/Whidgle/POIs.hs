{-
 - Whidgle.POIs
 -
 - Describes how Whidgle reasons about Points of Interest.
 -}
module Whidgle.POIs
( getPOIs
, distPOI
, scorePOI
) where

import Control.Lens

import Data.Function
import qualified Data.Map as M
import Data.Maybe

import Whidgle.Board
import Whidgle.Pathfinding
import Whidgle.Rules
import Whidgle.Temporal
import Whidgle.Types

-- Finds the distance to a Point of Interest.
distPOI :: POI -> Whidgle Int
distPOI (POI location _) = do
  route <- approach location
  return (maybe overwhelming distance route)

-- Determines the value of a Point of Interest.
scorePOI :: POI -> Whidgle Int
scorePOI (POI location inner) = do
  route <- approach location
  time <- use $ session.activityGame.gameTurn
  maxTime <- use $ session.activityGame.gameMaxTurns

  -- This is what we'll do to think about the route we find
  let
    continue r = do
      we <- use $ session.activityHero
      let acqTime = time + distance r
      -- determine our competition
      competition <- fmap
        -- who can't we fight?
        ( filter (not . (canFight (distance r) we))
        ) otherHeroes
      routes <- use $ session.activityGame.gameCompMap
      let
        lengths =
          -- who's reasonably close to us and the location when they pathfind?
          ( filter (all (< reasonablyClose))
          . map (\x ->
            let
            nearTo place =
              maybe overwhelming distance . ($ place) . (routes M.!) . (^.heroId) $ x
            in
            [nearTo location, nearTo (we^.heroPos)]
          )
          -- and is also actually here
          . not (^.heroCrashed)
          ) competition
      scoreBasic <- scoreMeta (distance r) acqTime inner

      return $ overTime time (min nearFuture (maxTime - time)) $
        if any (< distance r + 2) lengths -- + 2 -- assume the action takes one turn
          -- they can get us, so we lose our points
          then loseItAll we
          -- otherwise, nothing changes
          else scoreBasic

  -- If the route's impossible, very high constant cost -- otherwise, figure out
  -- how feasible it is.
  maybe (return (-overwhelming)) continue route

-- Scores a point of interest in the specific -- the above function converts this
-- specific score to a more general one based on the position of the POI and its
-- accessibility.
scoreMeta :: Int -> Int -> POIMeta -> Whidgle (Temporal Int)
scoreMeta dist _ (HasHero them) = do
  we <- use $ session.activityHero
  if canFight dist we them && dist < reasonablyClose
    then do
      ratio <- use $ internal.avoidanceRatios.at (them^.heroId).to (fromMaybe initialAvoidanceRatio)
      -- has attacking them worked well in the past?
      return $ if dist > 1 && shouldAttackRatio ratio
        then negative (loseItAll them)
        else constant 0 -- we just waste time if we pursue them
    else return $ loseItAll we

scoreMeta dist acq (HasMine) = do
  we <- use $ session.activityHero
  return $ if canTakeMine dist we
    -- give mines a slight positive score all the time if we can take them
    then timespan (gold 0.1) acq (acq + nearFuture) (constant (gold 1))
    -- but a major loss if we'd just die
    else loseItAll we

scoreMeta dist acq (HasTavern) = do
  we <- use $ session.activityHero
  return $
    case () of
     _-- if we need a drink, then we save all of our gold by going there
      | needsDrink dist we -> negative (loseItAll we)
      -- if we're at full life, then here's a huge penalty to keep us from
      -- hanging around
      | we^.heroLife >= tavernAvoid ->
        loseItAll we
      -- if we're far away then we'd be better off being near, given nothing else to do
      | dist > 1 -> constant (gold 0.1)
      -- let's just think in terms of the actual penalty
      | otherwise -> spike 0 acq $ gold (-2)

-- Figures out how much score we lose if we lose all mines.
loseItAll :: Hero -> Temporal Int
loseItAll our = constant (-(gold . fromIntegral $ (our^.heroMineCount)))

-- Figures out the score difference associated with a given gold value.
-- (TODO: Can we make this as generic as Num a => a -> Int without introducing
-- annoying extra typeclasses?)
gold :: (RealFrac a, Num a) => a -> Int
gold = (*10) . floor

-- Reads the board and determine what POIs are out there.
getPOIs :: Whidgle [POI]
getPOIs = do
  us <- use $ session.activityHero
  heroes <- otherHeroes
  board <- use $ session.activityGame.gameBoard
  return $ concat
    -- include all heroes that aren't us
    [ map poiHero heroes
    -- include any mines we don't own
    , seek (const HasMine) (notOurMine us) board
    -- include any taverns
    , seek (const HasTavern) isTavern board
    ]
  where
  poiHero h@(Hero{_heroPos = pos}) = POI pos (HasHero h)

  -- for mines: if it belongs to a hero, check their hero ID
  -- and make sure it's not ours
  notOurMine us (MineTile (Just h)) = h /= (us^.heroId)
  -- but if it doesn't belong to a hero it's definitely not ours
  notOurMine _ (MineTile Nothing) = True
  notOurMine _ _ = False

  isTavern TavernTile = True
  isTavern _ = False

  -- Looks around the board for tiles matching the predicate -- if so,
  -- builds them with the given constructor.
  seek constructor predicate board
    = map (\x -> POI (fst x) (constructor (snd x))) $
      isolate predicate board

otherHeroes :: Whidgle [Hero]
otherHeroes = do
  us <- use $ session.activityHero
  heroes <- use $ session.activityGame.gameHeroes
  -- other heroes are any heroes whose hero ID isn't ours
  return (filter (((/=) `on` (^.heroId)) us) heroes)
