{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.Internal.HTTP
  ( execute
  , executeExternalUpload
  , prepareRequest
  , parseResponse
  ) where

import Control.Exception (try)
import Data.Aeson
  ( FromJSON
  , Value (..)
  , eitherDecodeStrict'
  , encode
  , parseJSON
  )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither, parseMaybe, withObject, (.:), (.:?))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CI
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Network.HTTP.Client
  ( HttpException
  , Request
  , Response
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
  , setQueryString
  )
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
  ( hAccept
  , hAuthorization
  , hContentType
  , hRetryAfter
  , hUserAgent
  , methodGet
  , methodPost
  , statusCode
  )
import Network.HTTP.Types.URI (urlEncode)
import Text.Read (readMaybe)

import Network.Slack.Internal.Client
  ( Client (..)
  , ClientSettings (..)
  )
import Network.Slack.Types
  ( HttpVerb (..)
  , RequestBody (..)
  , ResponseMetadata (..)
  , SlackApiError (..)
  , SlackError (..)
  , SlackRequest (..)
  , SlackResponse (..)
  , methodText
  , tokenText
  )

execute :: FromJSON a => Client -> SlackRequest -> IO (Either SlackError (SlackResponse a))
execute client slackRequest = do
  result <- try $ do
    request <- prepareRequest client slackRequest
    httpLbs request (clientManager client)
  pure $ case result of
    Left exception -> Left (TransportError exception)
    Right response -> parseResponse response

prepareRequest :: Client -> SlackRequest -> IO Request
prepareRequest client slackRequest = do
  let settings = clientSettings client
      endpoint =
        clientBaseUrl settings
          <> ByteString.Char8.unpack (urlEncode True (Text.Encoding.encodeUtf8 (methodText (slackRequestMethod slackRequest))))
  initialRequest <- parseRequest endpoint
  let withMethod =
        initialRequest
          { method = case slackRequestVerb slackRequest of
              Get -> methodGet
              Post -> methodPost
          , requestHeaders = commonHeaders settings slackRequest <> requestHeaders initialRequest
          , responseTimeout = responseTimeoutMicro (clientTimeoutMicroseconds settings)
          }
  pure (applyBody (slackRequestVerb slackRequest) (slackRequestBody slackRequest) withMethod)

commonHeaders :: ClientSettings -> SlackRequest -> [(CI.CI ByteString, ByteString)]
commonHeaders settings slackRequest =
  [ (hAccept, "application/json")
  , (hUserAgent, clientUserAgent settings)
  ]
    <> maybe [] (\value -> [(hAuthorization, "Bearer " <> Text.Encoding.encodeUtf8 (tokenText value))]) (slackRequestToken slackRequest)

applyBody :: HttpVerb -> RequestBody -> Request -> Request
applyBody verb body request = case body of
  EmptyBody -> request
  FormBody parameters ->
    let encoded = map (\(key, value) -> (Text.Encoding.encodeUtf8 key, Just (Text.Encoding.encodeUtf8 value))) parameters
     in case verb of
          Get -> setQueryString encoded request
          Post ->
            request
              { requestHeaders = (hContentType, "application/x-www-form-urlencoded") : requestHeaders request
              , requestBody = HTTP.RequestBodyBS (renderForm parameters)
              }
  JsonBody value ->
    request
      { requestHeaders = (hContentType, "application/json; charset=utf-8") : requestHeaders request
      , requestBody = HTTP.RequestBodyLBS (encode value)
      }

renderForm :: [(Text, Text)] -> ByteString
renderForm =
  ByteString.Char8.intercalate "&"
    . map (\(key, value) -> urlEncode True (Text.Encoding.encodeUtf8 key) <> "=" <> urlEncode True (Text.Encoding.encodeUtf8 value))

parseResponse :: FromJSON a => Response LazyByteString.ByteString -> Either SlackError (SlackResponse a)
parseResponse response
  | statusCode status == 429 = Left (RateLimited retryAfter body)
  | statusCode status < 200 || statusCode status >= 300 = Left (HttpError status body)
  | otherwise = do
      value <- firstDecode body
      parseEnvelope value
  where
    status = responseStatus response
    body = LazyByteString.toStrict (responseBody response)
    retryAfter =
      fromMaybe 1 $ do
        raw <- lookup hRetryAfter (responseHeaders response)
        readMaybe (ByteString.Char8.unpack raw)

firstDecode :: ByteString -> Either SlackError Value
firstDecode body = case eitherDecodeStrict' body of
  Left message -> Left (DecodeError (Text.pack message) body)
  Right value -> Right value

parseEnvelope :: FromJSON a => Value -> Either SlackError (SlackResponse a)
parseEnvelope value = case parseEither envelopeParser value of
  Left message -> Left (DecodeError (Text.pack message) (LazyByteString.toStrict (encode value)))
  Right result -> result
  where
    envelopeParser = withObject "Slack Web API response" $ \object -> do
      ok <- object .: "ok"
      metadata <- metadataParser object
      if ok
        then Right . (`SlackResponse` metadata) <$> parseJSON value
        else do
          code <- object .:? "error" >>= pure . fromMaybe "unknown_error"
          pure (Left (ApiError (SlackApiError code metadata value)))

metadataParser :: KeyMap.KeyMap Value -> Parser ResponseMetadata
metadataParser object = do
  warnings <- fromMaybe [] <$> object .:? "warnings"
  singularWarning <- object .:? "warning"
  responseMetadata <- object .:? "response_metadata"
  let messages = fromMaybe [] $ do
        Object metadata <- responseMetadata
        parseMaybe (.:? "messages") metadata >>= id
      nestedWarnings = fromMaybe [] $ do
        Object metadata <- responseMetadata
        parseMaybe (.:? "warnings") metadata >>= id
      singularWarnings = maybe [] (map Text.strip . Text.splitOn ",") singularWarning
  pure
    ResponseMetadata
      { responseWarnings = nub (warnings <> singularWarnings <> nestedWarnings)
      , responseMessages = messages
      , responseMetadataValue = responseMetadata
      }

executeExternalUpload :: Client -> String -> ByteString -> IO (Either SlackError ())
executeExternalUpload client url contents = do
  result <- try $ do
    initialRequest <- parseRequest url
    let settings = clientSettings client
        request =
          initialRequest
            { method = methodPost
            , requestHeaders =
                [ (hContentType, "application/octet-stream")
                , (hUserAgent, clientUserAgent settings)
                ]
            , requestBody = HTTP.RequestBodyBS contents
            , responseTimeout = responseTimeoutMicro (clientTimeoutMicroseconds settings)
            }
    httpLbs request (clientManager client)
  pure $ case result of
    Left exception -> Left (TransportError (exception :: HttpException))
    Right response
      | let code = statusCode (responseStatus response), code >= 200 && code < 300 -> Right ()
      | otherwise ->
          Left
            ( HttpError
                (responseStatus response)
                (LazyByteString.toStrict (responseBody response))
            )
