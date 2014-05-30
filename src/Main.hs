{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Lens hiding (argument)

import Data.String (fromString)
import Data.Text (pack, unpack)

import Options.Applicative

import Network.Socket

import Whidgle.Bot
import Whidgle.Play
import Whidgle.Types

data Cmd = Training Settings (Maybe Int) (Maybe Board)
         | Arena Settings
         deriving (Show, Eq)

cmdSettings :: Cmd -> Settings
cmdSettings (Training s _ _) = s
cmdSettings (Arena s) = s

settings :: Parser Settings
settings = Settings <$> (Key <$> argument (Just . pack) (metavar "KEY"))
                    <*> (fromString <$> strOption (long "url" <> value "http://vindinium.org"))

trainingCmd :: Parser Cmd
trainingCmd = Training <$> settings
                       <*> optional (option (long "turns"))
                       <*> pure Nothing

arenaCmd :: Parser Cmd
arenaCmd = Arena <$> settings

cmd :: Parser Cmd
cmd = subparser
    ( command "training" (info trainingCmd
        ( progDesc "Run bot in training mode" ))
   <> command "arena" (info arenaCmd
        (progDesc "Run bot in arena mode" ))
    )

runCmd :: Cmd -> IO ()
runCmd c  = do
    s <- runWhidgle (cmdSettings c) (BotState undefined undefined) $
        case c of
            (Training _ t b) -> playTraining t b bot
            (Arena _)        -> playArena bot

    putStrLn $ "Game finished: " ++ unpack (s^.activityViewUrl)

main :: IO ()
main = withSocketsDo $
    execParser opts >>= runCmd
  where
    opts = info (cmd <**> helper) idm
