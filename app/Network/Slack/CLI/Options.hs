{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.CLI.Options
  ( Options (..)
  , optionsParser
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Options.Applicative
  ( Parser
  , argument
  , help
  , long
  , metavar
  , option
  , optional
  , str
  , strOption
  )

data Options = Options
  { optionToken :: Maybe Text
  , optionMethod :: Text
  , optionBody :: ByteString
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional (strOption (long "token" <> metavar "TOKEN" <> help "Slack token; prefer SLACK_TOKEN"))
    <*> argument str (metavar "METHOD" <> help "Web API method, for example conversations.list")
    <*> option (Text.encodeUtf8 . Text.pack <$> str) (long "json" <> metavar "JSON" <> help "JSON request object")
