{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Events API and interactive request decoding.
module Network.Slack.Events
  ( EventEnvelope (..)
  , decodeEvent
  , decodeInteractionPayload
  ) where

import Data.Aeson (FromJSON (parseJSON), Value, eitherDecodeStrict')
import Data.Aeson.Types (withObject, (.:), (.:?))
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Network.HTTP.Types.URI (parseQuery)

-- | Events API envelope, including a forward-compatible unknown case.
data EventEnvelope
  = UrlVerification Text
  | EventCallback Text Text Text Integer Value (Maybe Text) [Value]
  | UnknownEventEnvelope Text Value
  deriving stock (Eq, Show)

instance FromJSON EventEnvelope where
  parseJSON value = withObject "Slack Events API envelope" (parseObject value) value
    where
      parseObject original object = do
        envelopeType <- object .: "type"
        case envelopeType of
          "url_verification" -> UrlVerification <$> object .: "challenge"
          "event_callback" ->
            EventCallback
              <$> object .: "team_id"
              <*> object .: "api_app_id"
              <*> object .: "event_id"
              <*> object .: "event_time"
              <*> object .: "event"
              <*> object .:? "event_context"
              <*> (maybe [] id <$> object .:? "authorizations")
          _ -> pure (UnknownEventEnvelope envelopeType original)

-- | Decode an Events API JSON request after signature verification.
decodeEvent :: ByteString -> Either String EventEnvelope
decodeEvent = eitherDecodeStrict'

-- | Decode the @payload@ field from an interactive form request.
decodeInteractionPayload :: ByteString -> Either String Value
decodeInteractionPayload body = case listToMaybe [value | (key, Just value) <- parseQuery body, key == "payload"] of
  Nothing -> Left "form body does not contain a payload field"
  Just payload -> eitherDecodeStrict' payload
