{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}
-- |
-- Module      : Network.Slack.Api
--
-- Copyright   : (c) 2017 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
-- A Haskell Client for the Slack Web HTTP API
--
-----------------------------------------------------------------------------
module Network.Slack.Api
  ( SlackResponse(..)
  , endpoints
  , request
  , info
  , apiTest
  , authTest
  , channelsArchive
  , channelsCreate
  , channelsHistory
  , channelsInfo
  , channelsInvite
  , channelsJoin
  , channelsKick
  , channelsLeave
  , channelsList
  , channelsMark
  , channelsRename
  , channelsSetPurpose
  , channelsSetTopic
  , channelsUnarchive
  , chatDelete
  , chatPostMessage
  , chatUpdate
  , emojiList
  , filesDelete
  , filesInfo
  , filesList
  , filesUpload
  , groupsArchive
  , groupsClose
  , groupsCreate
  , groupsCreateChild
  , groupsHistory
  , groupsInvite
  , groupsKick
  , groupsLeave
  , groupsList
  , groupsMark
  , groupsOpen
  , groupsRename
  , groupsSetPurpose
  , groupsSetTopic
  , groupsUnarchive
  , imClose
  , imHistory
  , imList
  , imMark
  , imOpen
  , oauthAccess
  , rtmStart
  , searchAll
  , searchFiles
  , searchMessages
  , starsList
  , teamAccessLogs
  , userGetPresence
  , userInfo
  , userList
  , userSetActive
  , usersSetPresence
  ) where

import           Control.Applicative   ((<$>))
import           Control.Monad         (liftM, liftM2)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as L
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           Data.Semigroup        ((<>))

import           Network.Slack.Request (Endpoint, Token)
import qualified Network.Slack.Request as Req

type RequestParams = [(String, String)]

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

-------------------------------------------------------------------------
apiTest :: Token -> RequestParams -> IO SlackResponse
apiTest token params = request token "api.test" params

authTest :: Token -> RequestParams -> IO SlackResponse
authTest token params = request token "auth.test" params

channelsArchive :: Token -> RequestParams -> IO SlackResponse
channelsArchive token params = request token "channels.archive" params

channelsCreate :: Token -> RequestParams -> IO SlackResponse
channelsCreate token params = request token "channels.create" params

channelsHistory :: Token -> RequestParams -> IO SlackResponse
channelsHistory token params = request token "channels.history" params

channelsInfo :: Token -> RequestParams -> IO SlackResponse
channelsInfo token params = request token "channels.info" params

channelsInvite :: Token -> RequestParams -> IO SlackResponse
channelsInvite token params = request token "channels.invite" params

channelsJoin :: Token -> RequestParams -> IO SlackResponse
channelsJoin token params = request token "channels.join" params

channelsKick :: Token -> RequestParams -> IO SlackResponse
channelsKick token params = request token "channels.kick" params

channelsLeave :: Token -> RequestParams -> IO SlackResponse
channelsLeave token params = request token "channels.leave" params

channelsList :: Token -> RequestParams -> IO SlackResponse
channelsList token params = request token "channels.list" params

channelsMark :: Token -> RequestParams -> IO SlackResponse
channelsMark token params = request token "channels.mark" params

channelsRename :: Token -> RequestParams -> IO SlackResponse
channelsRename token params = request token "channels.rename" params

channelsSetPurpose :: Token -> RequestParams -> IO SlackResponse
channelsSetPurpose token params = request token "channels.setPurpose" params

channelsSetTopic :: Token -> RequestParams -> IO SlackResponse
channelsSetTopic token params = request token "channels.setTopic" params

channelsUnarchive :: Token -> RequestParams -> IO SlackResponse
channelsUnarchive token params = request token "channels.unarchive" params

chatDelete :: Token -> RequestParams -> IO SlackResponse
chatDelete token params = request token "chat.delete" params

chatPostMessage :: Token -> RequestParams -> IO SlackResponse
chatPostMessage token params = request token "chat.postMessage" params

chatUpdate :: Token -> RequestParams -> IO SlackResponse
chatUpdate token params = request token "chat.update" params

emojiList :: Token -> RequestParams -> IO SlackResponse
emojiList token params = request token "emoji.list" params

filesDelete :: Token -> RequestParams -> IO SlackResponse
filesDelete token params = request token "files.delete" params

filesInfo :: Token -> RequestParams -> IO SlackResponse
filesInfo token params = request token "files.info" params

filesList :: Token -> RequestParams -> IO SlackResponse
filesList token params = request token "files.list" params

filesUpload :: Token -> RequestParams -> IO SlackResponse
filesUpload token params = request token "files.upload" params

groupsArchive :: Token -> RequestParams -> IO SlackResponse
groupsArchive token params = request token "groups.archive" params

groupsClose :: Token -> RequestParams -> IO SlackResponse
groupsClose token params = request token "groups.close" params

groupsCreate :: Token -> RequestParams -> IO SlackResponse
groupsCreate token params = request token "groups.create" params

groupsCreateChild :: Token -> RequestParams -> IO SlackResponse
groupsCreateChild token params = request token "groups.createChild" params

groupsHistory :: Token -> RequestParams -> IO SlackResponse
groupsHistory token params = request token "groups.history" params

groupsInvite :: Token -> RequestParams -> IO SlackResponse
groupsInvite token params = request token "groups.invite" params

groupsKick :: Token -> RequestParams -> IO SlackResponse
groupsKick token params = request token "groups.kick" params

groupsLeave :: Token -> RequestParams -> IO SlackResponse
groupsLeave token params = request token "groups.leave" params

groupsList :: Token -> RequestParams -> IO SlackResponse
groupsList token params = request token "groups.list" params

groupsMark :: Token -> RequestParams -> IO SlackResponse
groupsMark token params = request token "groups.mark" params

groupsOpen :: Token -> RequestParams -> IO SlackResponse
groupsOpen token params = request token "groups.open" params

groupsRename :: Token -> RequestParams -> IO SlackResponse
groupsRename token params = request token "groups.rename" params

groupsSetPurpose :: Token -> RequestParams -> IO SlackResponse
groupsSetPurpose token params = request token "groups.setPurpose" params

groupsSetTopic :: Token -> RequestParams -> IO SlackResponse
groupsSetTopic token params = request token "groups.setTopic" params

groupsUnarchive :: Token -> RequestParams -> IO SlackResponse
groupsUnarchive token params = request token "groups.unarchive" params

imClose :: Token -> RequestParams -> IO SlackResponse
imClose token params = request token "im.close" params

imHistory :: Token -> RequestParams -> IO SlackResponse
imHistory token params = request token "im.history" params

imList :: Token -> RequestParams -> IO SlackResponse
imList token params = request token "im.list" params

imMark :: Token -> RequestParams -> IO SlackResponse
imMark token params = request token "im.mark" params

imOpen :: Token -> RequestParams -> IO SlackResponse
imOpen token params = request token "im.open" params

oauthAccess :: Token -> RequestParams -> IO SlackResponse
oauthAccess token params = request token "oauth.access" params

rtmStart :: Token -> RequestParams -> IO SlackResponse
rtmStart token params = request token "rtm.start" params

searchAll :: Token -> RequestParams -> IO SlackResponse
searchAll token params = request token "search.all" params

searchFiles :: Token -> RequestParams -> IO SlackResponse
searchFiles token params = request token "search.files" params

searchMessages :: Token -> RequestParams -> IO SlackResponse
searchMessages token params = request token "search.messages" params

starsList :: Token -> RequestParams -> IO SlackResponse
starsList token params = request token "stars.list" params

teamAccessLogs :: Token -> RequestParams -> IO SlackResponse
teamAccessLogs token params = request token "team.accessLogs" params

userGetPresence :: Token -> RequestParams -> IO SlackResponse
userGetPresence token params = request token "user.getPresence" params

userInfo :: Token -> RequestParams -> IO SlackResponse
userInfo token params = request token "user.info" params

userList :: Token -> RequestParams -> IO SlackResponse
userList token params = request token "user.list" params

userSetActive :: Token -> RequestParams -> IO SlackResponse
userSetActive token params = request token "user.setActive" params

usersSetPresence :: Token -> RequestParams -> IO SlackResponse
usersSetPresence token params = request token "users.setPresence" params
