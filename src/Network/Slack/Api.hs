{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- SLACK
--
-- Owain Lewis owain@owainlewis.com
-- A Haskell Client for the Slack Web HTTP API
--
-----------------------------------------------------------------------------
module Network.Slack.Api
  ( SlackResponse(..)
  , endpoints
  , request
  , info
  ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM, liftM2)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Semigroup((<>))

import qualified Network.Slack.Request as Req
import Network.Slack.Request(Endpoint, Token)

packParams :: [(String, String)] -> [(BS.ByteString, BS.ByteString)]
packParams = map (mapTuple C8.pack)
  where
    mapTuple f (x, y) = (f x, f y)

authEndpoints :: M.Map Endpoint String
authEndpoints =
  M.fromList
    [ ("api.test", "Checks API calling code")
    , ("auth.test", "Checks authentication & identity")
    ]

channelEndpoints :: M.Map Endpoint String
channelEndpoints =
  M.fromList
    [ ("channels.archive", "Archives a channel")
    , ("channels.create", "Creates a channel")
    , ( "channels.history"
      , "Fetches history of messages and events from a channel")
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
    ]

chatEndpoints :: M.Map Endpoint String
chatEndpoints =
  M.fromList
    [ ("chat.delete", "Deletes a message")
    , ("chat.postMessage", "Sends a message to a channel")
    , ("chat.update", "Updates a message")
    ]

emojiEndpoints :: M.Map Endpoint String
emojiEndpoints = M.fromList [("emoji.list", "Lists custom emoji for a team")]

fileEndpoints :: M.Map Endpoint String
fileEndpoints =
  M.fromList
    [ ("files.delete", "Seletes a file")
    , ("files.info", "Gets information about a team file")
    , ("files.list", "Lists & filters team files")
    , ("files.upload", "UPloads or creates a file")
    ]

groupEndpoints :: M.Map Endpoint String
groupEndpoints =
  M.fromList
    [ ("groups.archive", "Archives a private group")
    , ("groups.close", "Closes a private group")
    , ("groups.create", "Creates a private group")
    , ("groups.createChild", "Clones and archives a private group")
    , ( "groups.history"
      , "Fetches history of message and events from a private group")
    , ("groups.invite", "Invates a user to a private group")
    , ("groups.kick", "Removes a user from a private group")
    , ("groups.leave", "Leaves a private group")
    , ("groups.list", "List private groups that the user has access to")
    , ("groups.mark", "Sets the read cursor in a private group")
    , ("groups.open", "Opens a private group")
    , ("groups.rename", "Rename a private group")
    , ("groups.setPurpose", "Sets the purpose for a private group")
    , ("groups.setTopic", "Sets the topic for a private group")
    , ("groups.unarchive", "Unarchives a private group")
    ]

imEndpoints :: M.Map Endpoint String
imEndpoints =
  M.fromList
    [ ("im.close", "Close a direct message channel")
    , ( "im.history"
      , "Fetches history of messages and events from a direct message channel")
    , ("im.list", "Lists direct message channels for the calling user")
    , ("im.mark", "Sets the read cursor in a direct mesasge channel")
    , ("im.open", "Opens a direct message channel")
    ]

oauthEndpoints :: M.Map Endpoint String
oauthEndpoints =
  M.fromList
    [("oauth.access", "Exchanges a temporary OAuth code for an API token")]

rtmEndpoints :: M.Map Endpoint String
rtmEndpoints =
  M.fromList [("rtm.start", "Starts a Real Time Messaging session")]

searchEndpoints :: M.Map Endpoint String
searchEndpoints =
  M.fromList
    [ ("search.all", "Searches for messages and files matching a query")
    , ("search.files", "Searches for files matching a query")
    , ("search.messages", "Searches for messages matching a query")
    ]

starsEndpoints :: M.Map Endpoint String
starsEndpoints = M.fromList [("stars.list", "List stars for a user")]

teamEndpoints :: M.Map Endpoint String
teamEndpoints =
  M.fromList [("team.accessLogs", "Get the access logs for a team")]

userEndpoints :: M.Map Endpoint String
userEndpoints =
  M.fromList
    [ ("user.getPresence", "Gets user presence information")
    , ("user.info", "Gets information about a user")
    , ("user.list", "Lists all users in a Slack team")
    , ("user.setActive", "Marks a user as active")
    , ("users.setPresence", "Manually sets user presence")
    ]

endpoints :: M.Map Endpoint String
endpoints = M.unionsWith (<>) allEndpoints
  where
    allEndpoints =
      [ authEndpoints
      , channelEndpoints
      , chatEndpoints
      , emojiEndpoints
      , fileEndpoints
      , groupEndpoints
      , imEndpoints
      , oauthEndpoints
      , rtmEndpoints
      , searchEndpoints
      , starsEndpoints
      , teamEndpoints
      , userEndpoints
      ]

data SlackResponse
  = Success L.ByteString
  | InvalidEndpoint
  deriving (Show, Eq)

-- Perform a HTTP request to Slack and get a response
--
-- > request "TOKEN" "channels.list" []
--
request :: Req.Token -> String -> [(String, String)] -> IO SlackResponse
request token endpoint params =
  case M.lookup endpoint endpoints of
    Just _ -> do
      response <- Req.dispatch endpoint (packParams params) token
      return . Success $ response
    Nothing -> return InvalidEndpoint

mapKV
  :: (Ord k, Monad m)
  => (t -> m k) -> (t1 -> m a) -> M.Map t t1 -> m (M.Map k a)
mapKV kf vf = liftM M.fromList . mapM fs . M.assocs
  where
    fs (k, v) = liftM2 (,) (kf k) (vf v)

-- | Dumps out information about a Slack endpoint
--
-- > info "channels.list"
--
info :: String -> String
info endpoint = fromMaybe "Invalid enpoint" $ M.lookup endpoint endpoints
