{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Data.Aeson (Value, eitherDecodeStrict', encode)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Text qualified as Text
import Options.Applicative
  ( execParser
  , fullDesc
  , header
  , helper
  , info
  , progDesc
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
import Network.Slack.CLI.Options
  ( Options (optionBody, optionMethod, optionToken)
  , optionsParser
  )

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
