{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core request, response, credential, and error types.
module Network.Slack.Types
  ( Token
  , token
  , tokenText
  , SigningSecret
  , signingSecret
  , signingSecretBytes
  , Method
  , method
  , methodText
  , HttpVerb (..)
  , RequestBody (..)
  , SlackRequest (..)
  , SlackResponse (..)
  , ResponseMetadata (..)
  , SlackApiError (..)
  , SlackError (..)
  ) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client (HttpException)
import Network.HTTP.Types (Status)

-- | OAuth token used for Web API calls. Its 'Show' instance is redacted.
newtype Token = Token Text
  deriving newtype (Eq)

instance Show Token where
  show _ = "<Slack token>"

-- | Construct a Slack token.
token :: Text -> Token
token = Token

-- | Recover the token text when integrating with another client.
tokenText :: Token -> Text
tokenText (Token value) = value

-- | Secret used to verify requests from Slack. Its 'Show' instance is redacted.
newtype SigningSecret = SigningSecret ByteString
  deriving newtype (Eq)

instance Show SigningSecret where
  show _ = "<Slack signing secret>"

-- | Construct a Slack signing secret from its raw bytes.
signingSecret :: ByteString -> SigningSecret
signingSecret = SigningSecret

-- | Recover the secret bytes when integrating with another verifier.
signingSecretBytes :: SigningSecret -> ByteString
signingSecretBytes (SigningSecret value) = value

-- | Validated Slack Web API RPC method name.
newtype Method = Method Text
  deriving newtype (Eq, Ord)

instance Show Method where
  show (Method name) = Text.unpack name

-- | Validate a Web API method such as @conversations.list@.
method :: Text -> Either Text Method
method name
  | Text.null name = Left "Slack method cannot be empty"
  | Text.all validCharacter name = Right (Method name)
  | otherwise = Left "Slack method may contain only letters, digits, '.', '_', and '-'"
  where
    validCharacter character =
      (isAscii character && isAlphaNum character) || character `elem` ("._-" :: String)

-- | Return the validated method name.
methodText :: Method -> Text
methodText (Method name) = name

-- | HTTP verbs accepted by the Web API dispatcher.
data HttpVerb = Get | Post
  deriving stock (Eq, Show)

-- | One request encoding. Do not mix encodings in a Slack request.
data RequestBody
  = EmptyBody
  | FormBody [(Text, Text)]
  | JsonBody Value
  deriving stock (Eq, Show)

-- | Fully configurable Web API call.
data SlackRequest = SlackRequest
  { slackRequestMethod :: Method
  , slackRequestToken :: Maybe Token
  , slackRequestVerb :: HttpVerb
  , slackRequestBody :: RequestBody
  }
  deriving stock (Eq, Show)

-- | Non-fatal warnings and metadata returned beside an API result.
data ResponseMetadata = ResponseMetadata
  { responseWarnings :: [Text]
  , responseMessages :: [Text]
  , responseMetadataValue :: Maybe Value
  }
  deriving stock (Eq, Show)

-- | Successfully decoded API result and its metadata.
data SlackResponse a = SlackResponse
  { slackResponseValue :: a
  , slackResponseMetadata :: ResponseMetadata
  }
  deriving stock (Eq, Show)

-- | A response where Slack returned @"ok": false@.
data SlackApiError = SlackApiError
  { slackApiErrorCode :: Text
  , slackApiErrorMetadata :: ResponseMetadata
  , slackApiErrorResponse :: Value
  }
  deriving stock (Eq, Show)

-- | Every expected failure from an outbound Slack call.
data SlackError
  = TransportError HttpException
  | HttpError Status ByteString
  | RateLimited Int ByteString
  | DecodeError Text ByteString
  | ApiError SlackApiError

instance Show SlackError where
  show (TransportError exception) = "Slack transport error: " <> show exception
  show (HttpError status _) = "Slack HTTP error: " <> show status
  show (RateLimited seconds _) =
    "Slack rate limit exceeded; retry after " <> show seconds <> " seconds"
  show (DecodeError message _) = "Slack response decode error: " <> Text.unpack message
  show (ApiError apiError) =
    "Slack API error: " <> Text.unpack (slackApiErrorCode apiError)
