{-# LANGUAGE OverloadedStrings #-}

module Network.Slack.Api where

import qualified Data.ByteString.Char8   as B
import qualified Data.ByteString.Lazy    as L
import           Data.Monoid             (mconcat, (<>))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

makeRequest :: String -> String
makeRequest resource = mconcat [base, "/", resource]
    where base = "https://slack.com/api"

chatPostMessage = makeRequest "chat.postMessage"

type Endpoint = String
type Token = String

postRequest :: Endpoint -> Token -> [(B.ByteString, B.ByteString)] -> IO (Response L.ByteString)
postRequest url token bodyParams = do
    initReq <- parseUrl url
    let params = [("token", B.pack token)] ++ bodyParams
        request = urlEncodedBody params initReq
    withManager tlsManagerSettings $ httpLbs request

postWithBody :: Endpoint -> Token -> [(B.ByteString, B.ByteString)] -> IO L.ByteString
postWithBody url token bodyParams = do
  response <- postRequest url token bodyParams
  return $ responseBody response

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x,y) = (f x, f y)

packParams :: [(String, String)] -> [(B.ByteString, B.ByteString)]
packParams = map (mapTuple B.pack)

main token = postRequest chatPostMessage token (packParams [("channel", "#general"), ("message", "OH HELLO")])
