{-
 - Whidgle.Types
 -
 - Types and accessors for Whidgle.
 -}

 {-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
-- export everything: we have tons of accessors
module Whidgle.Types where

import Data.Function.Memoize
import qualified Data.Map as M
import Data.Text (Text)

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, Reader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)

data POI = POI Pos POIMeta
  deriving (Show)

data POIMeta
  = HasMine
  | HasHero Hero
  | HasTavern
  deriving (Show, Eq)

type CompositeMap = M.Map HeroId RouteMap
type RouteMap = (Pos -> Maybe Route)

newtype Route = Route [Pos]
  deriving (Show, Eq)

newtype Key = Key Text deriving (Show, Eq)

data Settings = Settings
  { _settingsKey :: Key
  , _settingsUrl :: Text
  } deriving (Show, Eq)

newtype Whidgle a = Whidgle { unWhidgle :: StateT Activity (ReaderT Settings IO) a }
  deriving
  ( Functor, Applicative, Monad
  , MonadReader Settings, MonadState Activity, MonadIO
  )

runWhidgle :: Settings -> Activity -> Whidgle a -> IO a
runWhidgle s st = flip runReaderT s . flip evalStateT st . unWhidgle

data Bot = Bot
  { initialize :: Whidgle ()
  , turn :: Whidgle Dir
  }

data Activity = Activity
  { _activityGame    :: Game
  , _activityHero    :: Hero
  , _activityToken   :: Text
  , _activityViewUrl :: Text
  , _activityPlayUrl :: Text
  }
newtype GameId = GameId Text
    deriving (Show, Eq)

data Game = Game
  { _gameId       :: GameId
  , _gameTurn     :: Int
  , _gameMaxTurns :: Int
  , _gameHeroes   :: [Hero]
  , _gameBoard    :: Board
  , _gameFinished :: Bool

  , _gameCompMap  :: CompositeMap
  }

newtype HeroId = HeroId Int
    deriving (Ord, Show, Eq)

data Hero = Hero
  { _heroId        :: HeroId
  , _heroName      :: Text
  , _heroUserId    :: Maybe Text
  , _heroElo       :: Maybe Int
  , _heroPos       :: Pos
  , _heroLife      :: Int
  , _heroGold      :: Int
  , _heroMineCount :: Int
  , _heroSpawnPos  :: Pos
  , _heroCrashed   :: Bool
  } deriving (Show, Eq)

data Board = Board
  { _boardSize  :: Int
  , _boardTiles :: [Tile]
  } deriving (Show, Eq)

data Tile = FreeTile
          | WoodTile
          | TavernTile
          | HeroTile HeroId
          | MineTile (Maybe HeroId)
    deriving (Show, Eq)

data Pos = Pos
  { _posX :: Int
  , _posY :: Int
  } deriving (Show, Eq, Ord)

data Dir = Stay | North | South | East | West
    deriving (Show, Eq)

deriveMemoizable ''Pos

makeLenses ''Settings
makeLenses ''Activity
makeLenses ''Game
makeLenses ''Hero
makeLenses ''Board
makeLenses ''Pos

activityHeroMap hId = activityGame.gameCompMap.at hId

-- unlensy getters
-- these are hard to define as lenses and we never need to use them as setters
fetchActivityOurMap :: Activity -> RouteMap
fetchActivityOurMap game =
  let
  am = game^.activityGame.gameCompMap
  us = _heroId (_activityHero game)
  in
  am M.! us
