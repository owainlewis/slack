{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Constant-time verification for requests sent by Slack.
module Network.Slack.Signature
  ( RequestTimestamp (..)
  , RequestSignature (..)
  , VerificationError (..)
  , verifyRequest
  , signRequest
  ) where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (constEq, convert)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Text.Read (readMaybe)

import Network.Slack.Types (SigningSecret, signingSecretBytes)

-- | Raw @X-Slack-Request-Timestamp@ header.
newtype RequestTimestamp = RequestTimestamp {unRequestTimestamp :: ByteString}
  deriving stock (Eq, Show)

-- | Raw @X-Slack-Signature@ header.
newtype RequestSignature = RequestSignature {unRequestSignature :: ByteString}
  deriving stock (Eq, Show)

-- | Reason an inbound request could not be authenticated.
data VerificationError
  = InvalidTimestamp
  | TimestampOutsideTolerance
  | SignatureMismatch
  deriving stock (Eq, Show)

-- | Reject stale requests, then compare the HMAC signature in constant time.
verifyRequest
  :: NominalDiffTime
  -> UTCTime
  -> SigningSecret
  -> RequestTimestamp
  -> RequestSignature
  -> ByteString
  -> Either VerificationError ()
verifyRequest tolerance now secret timestamp suppliedSignature body = do
  requestTime <-
    maybe
      (Left InvalidTimestamp)
      (Right . posixSecondsToUTCTime . fromInteger)
      (readMaybe (ByteString.unpack (unRequestTimestamp timestamp)))
  if abs (diffUTCTime now requestTime) > tolerance
    then Left TimestampOutsideTolerance
    else
      if unRequestSignature suppliedSignature `constEq` unRequestSignature (signRequest secret timestamp body)
        then Right ()
        else Left SignatureMismatch

-- | Compute Slack's version 0 HMAC-SHA256 request signature.
signRequest :: SigningSecret -> RequestTimestamp -> ByteString -> RequestSignature
signRequest secret timestamp body =
  RequestSignature ("v0=" <> convertToBase Base16 (convert digest :: ByteString))
  where
    baseString = "v0:" <> unRequestTimestamp timestamp <> ":" <> body
    digest = hmac (signingSecretBytes secret) baseString :: HMAC SHA256
