{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}
module Network.Slack.Request
  ( dispatch
  , Endpoint
  , Token
  ) where

import           Network.HTTP.Simple

import           Data.Aeson
import           Data.Aeson.Types      (parseMaybe)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as LBS
import           Data.Semigroup        ((<>))

type Endpoint = String

type Token = BS.ByteString

data Slack a = SlackSuccess a
             | SlackFailure

-- | Dispatches a request to the Slack API
--)
-- > dispatch "channels.list" [] "TOKEN"
--
dispatch :: Endpoint
         -> [(BS.ByteString, BS.ByteString)]
         -> Token
         -> IO LBS.ByteString
dispatch endpoint bodyParams token = do
  request' <- parseRequest (mkEndpoint endpoint)
  let params = [("token", token)] <> bodyParams
      request = setRequestBodyURLEncoded params $ request'
  response <- httpLBS request
  return $ getResponseBody response

--transformResponse :: Maybe Bool
transformResponse = do
    result <- decode "{\"ok\": true}"
    flip parseMaybe result $ \obj -> do
      status <- (obj .: "ok")
      if status then return (SlackSuccess result) else return SlackFailure

mkEndpoint :: String -> String
mkEndpoint = ("https://slack.com/api/" ++)
