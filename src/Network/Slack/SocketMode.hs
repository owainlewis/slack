{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Socket Mode connection lifecycle and acknowledgements.
module Network.Slack.SocketMode
  ( SocketEnvelope (..)
  , SocketHandlerResult (..)
  , SocketMessage (..)
  , SocketModeError (..)
  , SocketModeConfig (..)
  , defaultSocketModeConfig
  , decodeSocketMessage
  , socketAcknowledgement
  , dispatchSocketEnvelope
  , runSocketModeOnce
  , runSocketMode
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
  ( AsyncException
  , SomeException
  , displayException
  , fromException
  , throwIO
  , try
  )
import Data.Aeson
  ( FromJSON (parseJSON)
  , Value
  , eitherDecode
  , encode
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.URI
  ( URI (uriAuthority, uriPath, uriQuery, uriScheme)
  , URIAuth (uriPort, uriRegName)
  , parseURI
  )
import Network.WebSockets (Connection, receiveData, sendTextData)
import Network.HTTP.Types (statusCode)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Wuss (runSecureClient)

import Network.Slack.Client (Client)
import Network.Slack.Types
  ( Method
  , SlackApiError (slackApiErrorCode)
  , SlackError (..)
  , SlackResponse (slackResponseValue)
  , Token
  , method
  )
import Network.Slack.WebAPI (callJSON)

-- | An event or interactive payload delivered over Socket Mode.
data SocketEnvelope = SocketEnvelope
  { socketEnvelopeId :: Text
  , socketEnvelopeType :: Text
  , socketEnvelopePayload :: Value
  , socketEnvelopeAcceptsResponsePayload :: Bool
  }
  deriving stock (Eq, Show)

instance FromJSON SocketEnvelope where
  parseJSON = withObject "Slack Socket Mode envelope" $ \objectValue ->
    SocketEnvelope
      <$> objectValue .: "envelope_id"
      <*> objectValue .: "type"
      <*> objectValue .: "payload"
      <*> (maybe False id <$> objectValue .:? "accepts_response_payload")

-- | Response to acknowledge immediately and work to run after acknowledgement.
data SocketHandlerResult = SocketHandlerResult
  { socketResponsePayload :: Maybe Value
  , socketAfterAcknowledgement :: IO ()
  }

data OpenConnection = OpenConnection {openConnectionUrl :: String}

instance FromJSON OpenConnection where
  parseJSON = withObject "Slack Socket Mode connection" $ \objectValue ->
    OpenConnection <$> objectValue .: "url"

-- | Failure to open, parse, or use a Socket Mode connection.
data SocketModeError
  = SocketModeApiError SlackError
  | InvalidSocketModeUrl Text
  | SocketModeProtocolError Text
  | SocketModeConnectionError Text
  deriving stock (Show)

-- | Exponential reconnection policy for transient connection failures.
data SocketModeConfig = SocketModeConfig
  { socketModeInitialReconnectDelayMicroseconds :: Int
  , socketModeMaximumReconnectDelayMicroseconds :: Int
  }
  deriving stock (Eq, Show)

-- | Reconnect with jittered exponential backoff from one to thirty seconds.
defaultSocketModeConfig :: SocketModeConfig
defaultSocketModeConfig =
  SocketModeConfig
    { socketModeInitialReconnectDelayMicroseconds = 1000000
    , socketModeMaximumReconnectDelayMicroseconds = 30000000
    }

-- | Decode a Socket Mode control message or payload.
decodeSocketMessage :: ByteString -> Either String SocketMessage
decodeSocketMessage rawMessage = eitherDecode rawMessage >>= parseSocketMessage

-- | Build the acknowledgement required for a delivered envelope.
socketAcknowledgement :: SocketEnvelope -> Maybe Value -> Value
socketAcknowledgement envelope responsePayload =
  object
    ( ["envelope_id" .= socketEnvelopeId envelope]
        <> maybe [] (\payload -> ["payload" .= payload]) acceptedPayload
    )
  where
    acceptedPayload
      | socketEnvelopeAcceptsResponsePayload envelope = responsePayload
      | otherwise = Nothing

-- | Open one temporary connection and consume it until Slack disconnects.
runSocketModeOnce
  :: Client
  -> Token
  -> (SocketEnvelope -> SocketHandlerResult)
  -> IO (Either SocketModeError ())
runSocketModeOnce client appToken handler = do
  opened <- callJSON client appToken openConnectionMethod (object [])
  case opened of
    Left failure -> pure (Left (SocketModeApiError failure))
    Right response -> connect (openConnectionUrl (slackResponseValue response))
  where
    connect url = case socketLocation url of
      Left failure -> pure (Left failure)
      Right (host, port, path) -> do
        result <- trySynchronous (runSecureClient host (fromIntegral port) path (consume handler))
        pure $ case result of
          Left exception -> Left (SocketModeConnectionError (Text.pack (displayException exception)))
          Right (Left message) -> Left (SocketModeProtocolError message)
          Right (Right ()) -> Right ()

-- | Maintain connections until a terminal API, URL, or protocol error occurs.
--
-- Transient network failures use jittered exponential backoff. Async
-- cancellation always propagates to the caller.
runSocketMode
  :: SocketModeConfig
  -> Client
  -> Token
  -> (SocketEnvelope -> SocketHandlerResult)
  -> IO (Either SocketModeError ())
runSocketMode config client appToken handler =
  loop initialDelay
  where
    initialDelay = max 1 (socketModeInitialReconnectDelayMicroseconds config)
    maximumDelay = max initialDelay (socketModeMaximumReconnectDelayMicroseconds config)
    loop delay = do
      result <- runSocketModeOnce client appToken handler
      case result of
        Left (SocketModeApiError (RateLimited seconds _)) -> do
          threadDelay (max 0 seconds * 1000000)
          loop initialDelay
        Left failure@(SocketModeApiError slackFailure)
          | transientSlackError slackFailure -> reconnect delay
          | otherwise -> pure (Left failure)
        Left failure@(InvalidSocketModeUrl _) -> pure (Left failure)
        Left failure@(SocketModeProtocolError _) -> pure (Left failure)
        Left (SocketModeConnectionError _) -> reconnect delay
        Right () -> reconnect initialDelay
    reconnect delay = do
      jitteredDelay <- randomRIO (max 1 (delay `div` 2), max 1 delay)
      threadDelay jitteredDelay
      loop (delay + min delay (maximumDelay - delay))

consume :: (SocketEnvelope -> SocketHandlerResult) -> Connection -> IO (Either Text ())
consume handler connection = go
  where
    go = do
      rawMessage <- receiveData connection
      case decodeSocketMessage rawMessage of
        Left message -> pure (Left (Text.pack message))
        Right SocketHello -> go
        Right SocketDisconnect -> pure (Right ())
        Right (SocketPayload envelope) -> do
          dispatchSocketEnvelope (sendTextData connection . encode) handler envelope
          go

-- | Send an acknowledgement before starting the handler's follow-up work.
dispatchSocketEnvelope
  :: (Value -> IO ())
  -> (SocketEnvelope -> SocketHandlerResult)
  -> SocketEnvelope
  -> IO ()
dispatchSocketEnvelope sendAcknowledgement handler envelope = do
  let result = handler envelope
  sendAcknowledgement (socketAcknowledgement envelope (socketResponsePayload result))
  _ <- forkIO (socketAfterAcknowledgement result)
  pure ()

-- | Control message or delivered payload received from Slack.
data SocketMessage
  = SocketHello
  | SocketDisconnect
  | SocketPayload SocketEnvelope
  deriving stock (Eq, Show)

parseSocketMessage :: Value -> Either String SocketMessage
parseSocketMessage value = parseEither parser value
  where
    parser = withObject "Slack Socket Mode message" $ \objectValue -> do
      messageType <- objectValue .: "type" :: Parser Text
      case messageType of
        "hello" -> pure SocketHello
        "disconnect" -> pure SocketDisconnect
        _ -> SocketPayload <$> parseJSON value

socketLocation :: String -> Either SocketModeError (String, Int, String)
socketLocation rawUrl = do
  uri <- maybe (Left invalid) Right (parseURI rawUrl)
  if uriScheme uri /= "wss:"
    then Left invalid
    else do
      authority <- maybe (Left invalid) Right (uriAuthority uri)
      port <- parsePort (uriPort authority)
      pure (uriRegName authority, port, uriPath uri <> uriQuery uri)
  where
    invalid = InvalidSocketModeUrl (Text.pack rawUrl)
    parsePort "" = Right 443
    parsePort (':' : value) = maybe (Left invalid) Right (readMaybe value)
    parsePort _ = Left invalid

openConnectionMethod :: Method
openConnectionMethod = case method "apps.connections.open" of
  Right value -> value
  Left message -> error (show message)

trySynchronous :: IO value -> IO (Either SomeException value)
trySynchronous action = do
  result <- try action
  case result of
    Left exception -> case fromException exception :: Maybe AsyncException of
      Just asynchronous -> throwIO asynchronous
      Nothing -> pure (Left exception)
    Right value -> pure (Right value)

transientSlackError :: SlackError -> Bool
transientSlackError (TransportError _) = True
transientSlackError (HttpError status _) = statusCode status >= 500
transientSlackError (RateLimited _ _) = True
transientSlackError (ApiError apiError) =
  slackApiErrorCode apiError
    `elem` ["fatal_error", "internal_error", "request_timeout", "service_unavailable"]
transientSlackError (DecodeError _ _) = False
