{-# LANGUAGE OverloadedStrings #-}

-- | Future-proof dispatch for every Slack Web API method.
module Network.Slack.WebAPI
  ( call
  , callJSON
  , callForm
  , callUnauthenticatedJSON
  , withRawResponse
  , postWebhook
  ) where

import Control.Exception (try)
import Data.Aeson (FromJSON, ToJSON, Value, encode, toJSON)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Network.HTTP.Client
  ( HttpException
  , BodyReader
  , RequestBody (RequestBodyLBS)
  , brConsume
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseHeaders
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  , withResponse
  )
import Network.HTTP.Types
  ( ResponseHeaders
  , Status
  , hContentType
  , hAccept
  , hRetryAfter
  , hUserAgent
  , methodPost
  , statusCode
  )
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Network.Slack.Internal.Client
  ( Client (clientManager, clientSettings)
  , ClientSettings (clientTimeoutMicroseconds, clientUserAgent)
  )
import Network.Slack.Internal.HTTP (execute, prepareRequest)
import Network.Slack.Types
  ( HttpVerb (Post)
  , Method
  , RequestBody (FormBody, JsonBody)
  , SlackError (..)
  , SlackRequest (..)
  , SlackResponse
  , Token
  )

-- | Execute a fully specified Web API request.
call :: FromJSON a => Client -> SlackRequest -> IO (Either SlackError (SlackResponse a))
call = execute

-- | Call an authenticated method with a JSON body.
callJSON :: (ToJSON request, FromJSON response) => Client -> Token -> Method -> request -> IO (Either SlackError (SlackResponse response))
callJSON client authToken apiMethod body =
  call
    client
    SlackRequest
      { slackRequestMethod = apiMethod
      , slackRequestToken = Just authToken
      , slackRequestVerb = Post
      , slackRequestBody = JsonBody (toJSON body)
      }

-- | Call an authenticated method with form-encoded parameters.
callForm :: FromJSON response => Client -> Token -> Method -> [(Text, Text)] -> IO (Either SlackError (SlackResponse response))
callForm client authToken apiMethod body =
  call
    client
    SlackRequest
      { slackRequestMethod = apiMethod
      , slackRequestToken = Just authToken
      , slackRequestVerb = Post
      , slackRequestBody = FormBody body
      }

-- | Call a method such as an OAuth exchange without bearer authentication.
callUnauthenticatedJSON :: (ToJSON request, FromJSON response) => Client -> Method -> request -> IO (Either SlackError (SlackResponse response))
callUnauthenticatedJSON client apiMethod body =
  call
    client
    SlackRequest
      { slackRequestMethod = apiMethod
      , slackRequestToken = Nothing
      , slackRequestVerb = Post
      , slackRequestBody = JsonBody (toJSON body)
      }

-- | Stream a non-JSON Web API response, such as @admin.analytics.getFile@.
--
-- The callback must consume the 'BodyReader' before it returns. Non-success
-- statuses and rate limits are converted to 'SlackError' before the callback.
withRawResponse
  :: Client
  -> SlackRequest
  -> (Status -> ResponseHeaders -> BodyReader -> IO result)
  -> IO (Either SlackError result)
withRawResponse client slackRequest consumeResponse = do
  result <- try $ do
    preparedRequest <- prepareRequest client slackRequest
    let request =
          preparedRequest
            { requestHeaders =
                (hAccept, "*/*")
                  : filter ((/= hAccept) . fst) (requestHeaders preparedRequest)
            }
    withResponse request (clientManager client) $ \response -> do
      let status = responseStatus response
          headers = responseHeaders response
      if statusCode status == 429
        then do
          body <- readBody (responseBody response)
          let retryAfter =
                fromMaybe 1 $ do
                  raw <- lookup hRetryAfter headers
                  readMaybe (ByteString.Char8.unpack raw)
          pure (Left (RateLimited retryAfter body))
        else
          if statusCode status < 200 || statusCode status >= 300
            then Left . HttpError status <$> readBody (responseBody response)
            else Right <$> consumeResponse status headers (responseBody response)
  pure $ case result of
    Left exception -> Left (TransportError (exception :: HttpException))
    Right response -> response

readBody :: BodyReader -> IO ByteString.ByteString
readBody reader = ByteString.concat <$> brConsume reader

-- | Send a JSON payload to a Slack incoming webhook URL.
postWebhook :: Client -> String -> Value -> IO (Either SlackError ())
postWebhook client url value = do
  result <- try $ do
    initialRequest <- parseRequest url
    let request =
          initialRequest
            { method = methodPost
            , requestHeaders =
                [ (hContentType, "application/json; charset=utf-8")
                , (hUserAgent, clientUserAgent (clientSettings client))
                ]
                  <> requestHeaders initialRequest
            , requestBody = RequestBodyLBS (encode value)
            , responseTimeout = responseTimeoutMicro (clientTimeoutMicroseconds (clientSettings client))
            }
    httpLbs request (clientManager client)
  pure $ case result of
    Left exception -> Left (TransportError (exception :: HttpException))
    Right response
      | let code = statusCode (responseStatus response), code >= 200 && code < 300 -> Right ()
      | otherwise -> Left (HttpError (responseStatus response) (LazyByteString.toStrict (responseBody response)))
