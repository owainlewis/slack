{-# LANGUAGE OverloadedStrings #-}
module Network.Slack.Api
    ( SlackResponse(..)
    , makeRequest
    , endpoints
    , request
     ) where

import           Control.Applicative     ((<$>))
import qualified Data.ByteString.Char8   as B
import qualified Data.ByteString.Lazy    as L
import qualified Data.Map                as M
import           Data.Monoid             (mconcat, (<>))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

makeRequest :: String -> String
makeRequest resource = mconcat [base, "/", resource]
    where base = "https://slack.com/api"

type Endpoint = String
type Token    = String

postRequest :: Endpoint -> Token -> [(B.ByteString, B.ByteString)] -> IO (Response L.ByteString)
postRequest url token bodyParams = do
    initReq <- parseUrl url
    let params = [("token", B.pack token)] ++ bodyParams
        request = urlEncodedBody params initReq
    withManager tlsManagerSettings $ httpLbs request

postWithBody :: Endpoint -> Token -> [(B.ByteString, B.ByteString)] -> IO L.ByteString
postWithBody url token bodyParams = do
  response <- postRequest url token bodyParams
  return $ responseBody response

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x,y) = (f x, f y)

packParams :: [(String, String)] -> [(B.ByteString, B.ByteString)]
packParams = map (mapTuple B.pack)

endpoints :: M.Map String String
endpoints = M.fromList
-- Auth
    [ ("api.test", "Checks API calling code")
    , ("auth.test", "Checks authentication & identity")
-- Channels
    , ("channels.archive", "Archives a channel")
    , ("channels.create", "Creates a channel")
    , ("channels.history", "Fetches history of messages and events from a channel")
    , ("channels.info", "Gets information about a channel")
    , ("channels.invite", "Invite users to a channel")
    , ("channels.join", "Joins a channel, creating if required")
    , ("channels.kick", "Removes a user from a channel")
    , ("channels.leave", "Leaves a channel")
    , ("channels.list", "Lists all channels in a Slack team")
    , ("channels.mark", "Sets the read cursor in a channel")
    , ("channels.rename", "Renames a channel")
    , ("channels.setPurpose", "Sets the purpose for a channel")
    , ("channels.setTopic", "Sets the topic for a channel")
    , ("channels.unarchive", "Unarchives a channel")
-- Chat
    , ("chat.delete", "Deletes a message")
    , ("chat.postMessage", "Sends a message to a channel")
    , ("chat.update", "Updates a message")
    ]

data SlackResponse = Success L.ByteString | InvalidEndpoint
    deriving ( Show, Eq )

-- Run a request and return the body
--
runRequest :: Token -> String -> [(String, String)] -> IO L.ByteString
runRequest token endpoint params =
    postWithBody fullRequestPath token normalizedParams
    where fullRequestPath = makeRequest endpoint
          normalizedParams = packParams params

-- Perform a HTTP request to Slack and get a response
--
request :: Token -> String -> [(String, String)] -> IO SlackResponse
request token endpoint params =
  case M.lookup endpoint endpoints of
    Just _ -> do
      response <- runRequest token endpoint params
      return . Success $ response
    Nothing -> return InvalidEndpoint
