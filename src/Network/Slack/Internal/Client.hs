{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.Internal.Client
  ( Client (..)
  , ClientSettings (..)
  , defaultClientSettings
  , newClient
  , newClientWith
  ) where

import Data.ByteString (ByteString)
import Network.HTTP.Client (Manager, ManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Base URL, user agent, and timeout for outbound requests.
data ClientSettings = ClientSettings
  { clientBaseUrl :: String
  , clientUserAgent :: ByteString
  , clientTimeoutMicroseconds :: Int
  }
  deriving stock (Eq, Show)

-- | Shared HTTP client. Reuse one value to reuse its connection pool.
data Client = Client
  { clientManager :: Manager
  , clientSettings :: ClientSettings
  }

-- | Production settings for @https://slack.com/api/@ with a 30-second timeout.
defaultClientSettings :: ClientSettings
defaultClientSettings =
  ClientSettings
    { clientBaseUrl = "https://slack.com/api/"
    , clientUserAgent = "slack-hs/1.0"
    , clientTimeoutMicroseconds = 30 * 1000 * 1000
    }

-- | Create a TLS-enabled client with 'defaultClientSettings'.
newClient :: IO Client
newClient = newClientWith tlsManagerSettings defaultClientSettings

-- | Create a client with custom manager and request settings.
newClientWith :: ManagerSettings -> ClientSettings -> IO Client
newClientWith managerSettings settings = do
  manager <- newManager managerSettings
  pure Client {clientManager = manager, clientSettings = settings}

instance Show Client where
  show Client {clientSettings = settings} = "Client " <> show settings
