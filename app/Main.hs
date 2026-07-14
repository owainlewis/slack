{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Data.Aeson (Value, eitherDecodeStrict', encode)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative
  ( Parser
  , argument
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , optional
  , progDesc
  , str
  , strOption
  )
import System.Environment (lookupEnv)

import Network.Slack
  ( SlackError
  , SlackResponse (slackResponseValue)
  , callJSON
  , method
  , newClient
  , token
  )

data Options = Options
  { optionToken :: Maybe Text
  , optionMethod :: Text
  , optionBody :: ByteString.ByteString
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional (strOption (long "token" <> metavar "TOKEN" <> help "Slack token; prefer SLACK_TOKEN"))
    <*> argument str (metavar "METHOD" <> help "Web API method, for example conversations.list")
    <*> option (ByteString.pack <$> str) (long "json" <> metavar "JSON" <> help "JSON request object")

main :: IO ()
main = do
  options <-
    execParser
      ( info
          (helper <*> optionsParser)
          (fullDesc <> progDesc "Call any Slack Web API method" <> header "slack-api")
      )
  environmentToken <- fmap Text.pack <$> lookupEnv "SLACK_TOKEN"
  authToken <- maybe (fail "set SLACK_TOKEN or pass --token") pure (optionToken options <|> environmentToken)
  case (method (optionMethod options), eitherDecodeStrict' (optionBody options)) of
    (Left message, _) -> fail (show message)
    (_, Left message) -> fail message
    (Right apiMethod, Right body) -> do
      client <- newClient
      response <- callJSON client (token authToken) apiMethod (body :: Value)
      render response

render :: Either SlackError (SlackResponse Value) -> IO ()
render (Left failure) = fail (show failure)
render (Right response) = LazyByteString.putStrLn (encode (slackResponseValue response))
