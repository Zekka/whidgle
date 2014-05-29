{-# LANGUAGE OverloadedStrings #-}
{-
 - Whidgle.Api
 -
 - Taken mostly from the original Vindinium starter. Describes networking for Vindinium.
 -}
module Whidgle.Api
( startTraining
, startArena
, move
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader
import Control.Lens hiding ((.=))

import Data.Aeson
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)

import Network.HTTP.Client
import Network.HTTP.Types

import Whidgle.CompositeMap
import Whidgle.Types

-- Starts training the bot.
startTraining :: Maybe Int -> Maybe Board -> Whidgle Activity
startTraining mi mb = do
  url <- startUrl "training"
  let
    obj = object
      (  maybe [] (\i -> [("turns", toJSON i)]) mi
      <> maybe [] (\b -> [("map",  toJSON b)]) mb
      )

  request url obj

-- Moves the bot in a direction, sending it to the server and
-- resulting in the new Activity.
move :: Activity -> Dir -> Whidgle Activity
move s d = do
  let
    url = s^.activityPlayUrl
    obj = object [("dir", toJSON d)]

  request url obj

-- Starts an arena run with the bot.
startArena :: Whidgle Activity
startArena = do
  url <- startUrl "arena"
  let obj = object []

  request url obj

-- Starts the bot with a given URL.
startUrl :: Text -> Whidgle Text
startUrl v = liftM (\x -> x <> "/api/" <> v) $ asks _settingsUrl

-- Makes a request to a URL, resulting in a new Activity.
request :: Text -> Value -> Whidgle Activity
request url val = do
  key <- asks _settingsKey

  initReq <- liftIO $ parseUrl $ unpack url
  let
    req = initReq
      { method = "POST"
      , requestHeaders =
        [ (hContentType, "application/json")
        , (hAccept,      "application/json")
        , (hUserAgent,   "whidgle")
        ]
      , requestBody = jsonBody (injectKey val key)
      }

  liftIO $ withManager defaultManagerSettings $ \mgr ->
    liftM (decodeBody . responseBody) $ httpLbs req mgr

  where
  jsonBody = RequestBodyLBS . encode
  decodeBody body = case eitherDecode body of
    Left e  -> error $ "request: unable to decode state: " ++ e
    Right s -> s

  -- Adds our key to the POSTed object.
  injectKey (Object a) k =
    let (Object b) = object [("key", toJSON k)] in
      Object (a <> b)

-- Parses the board.
parseBoard :: Int -> String -> Board
parseBoard s t = Board s $ map parse (chunks t)
  where
  -- Turns the board into a bunch of two-character chunks.
  chunks []       = []
  chunks (_:[])   = error "chunks: even chars number"
  chunks (a:b:xs) = (a, b): chunks xs

  parse (' ', ' ') = FreeTile
  parse ('#', '#') = WoodTile
  parse ('@', x)   = HeroTile $ HeroId $ read [x]
  parse ('[', ']') = TavernTile
  parse ('$', '-') = MineTile Nothing
  parse ('$', x)   = MineTile $ Just $ HeroId $ read [x]
  parse (a, b)     = error $ "parse: unknown tile pattern " ++ (show $ a:b:[])

-- Serializes the board.
printTiles :: [Tile] -> Text
printTiles =
  foldl (<>) "" . map printTile
  where
  printTile FreeTile = "  "
  printTile WoodTile = "##"
  printTile (HeroTile (HeroId i)) = "@" <> (pack $ show i)
  printTile TavernTile = "[]"
  printTile (MineTile Nothing) = "$-"
  printTile (MineTile (Just (HeroId i))) = "$" <> (pack $ show i)

-- Schema for received data.
instance ToJSON Key where
  toJSON (Key k) = String k

instance ToJSON Board where
  toJSON b =
    object
      [ "size"  .= _boardSize b
      , "tiles" .= (printTiles $ _boardTiles b)
      ]

instance FromJSON Activity where
  parseJSON (Object o) = Activity
    <$> o .: "game"
    <*> o .: "hero"
    <*> o .: "token"
    <*> o .: "viewUrl"
    <*> o .: "playUrl"
  parseJSON _ = mzero

instance FromJSON Game where
  parseJSON (Object o) = Game
    <$> o .: "id"
    <*> o .: "turn"
    <*> o .: "maxTurns"
    <*> o .: "heroes"
    <*> o .: "board"
    <*> o .: "finished"

    -- Hack: includes the generated maps in the board state so that they're cached
    <*> (generateMaps <$> (o .: "board") <*> (o .: "heroes"))

  parseJSON _ = mzero

instance FromJSON GameId where
  parseJSON x = GameId <$> parseJSON x

instance FromJSON Hero where
  parseJSON (Object o) = Hero
    <$> o .: "id"
    <*> o .: "name"
    <*> o .:? "userId"
    <*> o .:? "elo"
    <*> o .: "pos"
    <*> o .: "life"
    <*> o .: "gold"
    <*> o .: "mineCount"
    <*> o .: "spawnPos"
    <*> o .: "crashed"
  parseJSON _ = mzero

instance FromJSON HeroId where
  parseJSON x = HeroId <$> parseJSON x

instance FromJSON Pos where
  parseJSON (Object o) = Pos <$> o .: "x" <*> o .: "y"
  parseJSON _ = mzero

instance FromJSON Board where
  parseJSON (Object o) = parseBoard <$> o .: "size" <*> o .: "tiles"
  parseJSON _ = mzero

instance ToJSON Dir where
  toJSON Stay = String "Stay"
  toJSON North = String "North"
  toJSON South = String "South"
  toJSON East = String "East"
  toJSON West = String "West"
