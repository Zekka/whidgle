{-
 - Whidgle.Rules
 -
 - A mixture of game rules and heuristics.
 -}
-- export everything
module Whidgle.Rules where

import Control.Lens

import Data.Function

import Whidgle.Types

-- How many steps away an opponent can be before we consider the possibility of attack behavior.
reasonablyClose :: Int
reasonablyClose = 10

-- The number of turns to plan for in the future.
nearFuture :: Int
nearFuture = 300

-- Constants dealing to avoidance of enemies.
attackDamage, wiggleRoom, gobboRoom, tavernRoom, tavernAvoid :: Int
attackDamage = 20 -- assume attacks do at least this much damage
wiggleRoom = 10 -- don't fight without at least this much wiggle-room
gobboRoom = 10 -- with less than this health, avoid gobbos
tavernRoom = 90 -- with less than this health, drink when near a tavern
tavernAvoid = 95 -- with more than this health, never drink near a tavern

-- Determine whether we need a drink.
needsDrink :: Int -> Hero -> Bool
-- be far more willing to drink when near a tavern than when far
-- dist * 5 : assumes about an opportunity cost of 2.5g/tile when drinking
needsDrink dist us =
  us^.heroLife <= max (tavernRoom - dist * 5) wiggleRoom

-- Determines whether a hero should assail another hero.
-- (minding that we can't trust others to follow the same logic)
canFight :: Int -> Hero -> Hero -> Bool
canFight dist assailant target =
  assailant^.heroLife - dist > target^.heroLife + wiggleRoom + attackDamage

-- Determines whether a hero can even assail another hero. If it's false, the
-- assailant will just get maimed to death.
canEverFight :: Int -> Hero -> Hero -> Bool
canEverFight dist assailant target =
  assailant^.heroLife - dist > target^.heroLife + attackDamage

-- Determines whether a hero can take a mine without dying.
canTakeMine :: Int -> Hero -> Bool
canTakeMine dist us =
  us^.heroLife - dist > gobboRoom + attackDamage

-- The score which indicates a venue is no longer worth pursuing, or that it must be pursued.
-- Think of it as an unenforced ceiling.
overwhelming :: Int
overwhelming = 1000

-- returns true if we should attack something given their current avoidance ratio
shouldAttackRatio :: (Int, Int) -> Bool
shouldAttackRatio = (<0.7) . uncurry ((/) `on` fromIntegral) -- require 7/10 misses at most

initialAvoidanceRatio :: (Int, Int)
initialAvoidanceRatio = (0, 2) -- begin assuming we've successfully attacked twice
