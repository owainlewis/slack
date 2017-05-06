{-# LANGUAGE OverloadedStrings #-}
module Network.Slack.Request
    ( dispatch
    , Endpoint
    , Token
    ) where

import Network.HTTP.Simple

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Data.Semigroup((<>))

type Endpoint = String
type Token = String

-- | Dispatches a request to the Slack API
--
-- > dispatch "channels.list" [] "TOKEN"
--
dispatch :: Endpoint ->
            [(BS.ByteString, BS.ByteString)] ->
            Token ->
            IO LBS.ByteString
dispatch endpoint bodyParams token = do
    request' <- parseRequest ("https://slack.com/api/" <> endpoint)
    let params = [("token", C8.pack token)] <> bodyParams
        request =
          setRequestBodyURLEncoded params $
          request'
    response <- httpLBS request
    return $ getResponseBody response
