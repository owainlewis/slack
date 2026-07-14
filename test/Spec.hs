{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Data.Aeson (Value, eitherDecodeStrict', object, (.=))
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Client
  ( HttpException (InvalidUrlException)
  , brConsume
  , defaultManagerSettings
  )
import Network.HTTP.Types
  ( hAuthorization
  , hAccept
  , hContentType
  , hRetryAfter
  , status200
  , status429
  , status500
  )
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaderHost
  , requestHeaders
  , responseLBS
  , strictRequestBody
  )
import Network.Wai.Handler.Warp (testWithApplication)
import Options.Applicative
  ( ParserResult (CompletionInvoked, Failure, Success)
  , defaultPrefs
  , execParserPure
  , fullDesc
  , info
  )
import Test.Hspec
  ( Spec
  , describe
  , expectationFailure
  , hspec
  , it
  , shouldBe
  , shouldNotContain
  , shouldSatisfy
  )

import Network.Slack
import Network.Slack.CLI.Options (Options (optionBody), optionsParser)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CLI" $ do
    it "preserves non-ASCII JSON as UTF-8" $ do
      case execParserPure defaultPrefs (info optionsParser fullDesc) ["chat.postMessage", "--json", "{\"text\":\"Hello 👋 世界\"}"] of
        Success options ->
          eitherDecodeStrict' (optionBody options)
            `shouldBe` Right (object ["text" .= ("Hello 👋 世界" :: Text)])
        Failure _ -> expectationFailure "CLI options unexpectedly failed to parse"
        CompletionInvoked _ -> expectationFailure "CLI options unexpectedly requested completion"

  describe "credentials" $ do
    it "redacts tokens and signing secrets" $ do
      show (token "xoxb-secret") `shouldBe` "<Slack token>"
      show (signingSecret "secret") `shouldBe` "<Slack signing secret>"

  describe "Web API transport" $ do
    it "uses bearer auth and JSON for arbitrary methods" $
      testWithApplication (pure mockSlack) $ \port -> do
        client <- mockClient port
        apiMethod <- requireMethod "admin.conversations.restrictAccess.addGroup"
        response <- callJSON client (token "xoxb-secret") apiMethod (object ["channel_id" .= ("C1" :: Text)])
        case response of
          Left failure -> expectationFailure (show failure)
          Right success ->
            slackResponseValue success
              `shouldBe` object ["ok" .= True, "method" .= ("admin.conversations.restrictAccess.addGroup" :: Text)]
    it "returns Slack API errors and rate limits as data" $
      testWithApplication (pure mockSlack) $ \port -> do
        client <- mockClient port
        denied <- (requireMethod "denied.method" >>= \apiMethod -> callJSON client (token "x") apiMethod (object [])) :: IO (Either SlackError (SlackResponse Value))
        showEither denied `shouldBe` "Slack API error: missing_scope"
        limited <- (requireMethod "rate.limited" >>= \apiMethod -> callJSON client (token "x") apiMethod (object [])) :: IO (Either SlackError (SlackResponse Value))
        showEither limited `shouldBe` "Slack rate limit exceeded; retry after 7 seconds"
    it "streams non-JSON method responses without buffering in the SDK" $
      testWithApplication (pure mockSlack) $ \port -> do
        client <- mockClient port
        apiMethod <- requireMethod "admin.analytics.getFile"
        result <-
          withRawResponse
            client
            SlackRequest
              { slackRequestMethod = apiMethod
              , slackRequestToken = Just (token "x")
              , slackRequestVerb = Get
              , slackRequestBody = EmptyBody
              }
            (\_ _ reader -> ByteString.concat <$> brConsume reader)
        case result of
          Left failure -> expectationFailure (show failure)
          Right body -> body `shouldBe` "gzip-ndjson-bytes"
    it "collects and deduplicates singular and structured warnings" $
      testWithApplication (pure mockSlack) $ \port -> do
        client <- mockClient port
        apiMethod <- requireMethod "warning.method"
        result <- callJSON client (token "x") apiMethod (object []) :: IO (Either SlackError (SlackResponse Value))
        case result of
          Left failure -> expectationFailure (show failure)
          Right response ->
            responseWarnings (slackResponseMetadata response) `shouldBe` ["legacy_field", "other_warning"]
    it "posts incoming webhook JSON" $
      testWithApplication (pure mockSlack) $ \port -> do
        client <- mockClient port
        result <- postWebhook client ("http://127.0.0.1:" <> show port <> "/hook") (object ["text" .= ("hello" :: Text)])
        showEither result `shouldBe` "success"
    it "redacts incoming webhook credentials from transport errors" $ do
      let failure =
            TransportError
              ( InvalidUrlException
                  "https://hooks.slack.com/services/T123/B456/super-secret?token=also-secret"
                  "redirected to https://example.com/super-secret"
              )
      show failure `shouldBe` "Slack transport error"
      show failure `shouldNotContain` "super-secret"
      show failure `shouldNotContain` "also-secret"

  describe "external file uploads" $ do
    it "runs get URL, raw upload, and completion in order" $ do
      stages <- newIORef ([] :: [(Text, LazyByteString.ByteString)])
      testWithApplication (pure (mockUpload stages)) $ \port -> do
        client <- mockClient port
        result <-
          uploadFile
            client
            (token "xoxb-secret")
            FileUpload
              { fileUploadName = "report.txt"
              , fileUploadContents = "file bytes"
              , fileUploadTitle = Just "Report"
              , fileUploadAltText = Nothing
              , fileUploadSnippetType = Just "text"
              , fileUploadChannelId = Just "C1"
              , fileUploadInitialComment = Nothing
              , fileUploadThreadTimestamp = Nothing
              }
        case result of
          Left failure -> expectationFailure (show failure)
          Right files -> files `shouldBe` [UploadedFile "F1" (Just "Report")]
        recorded <- readIORef stages
        map fst recorded `shouldBe` ["get-url", "upload", "complete"]
        lookup "upload" recorded `shouldBe` Just "file bytes"

  describe "method" $ do
    it "accepts every valid Slack RPC method shape" $ do
      method "admin.conversations.restrictAccess.addGroup" `shouldSatisfy` isRight
    it "rejects paths and query strings" $ do
      method "chat.postMessage/../../token" `shouldSatisfy` isLeft
      method "chat.postMessage?token=secret" `shouldSatisfy` isLeft

  describe "request signatures" $ do
    let secret = signingSecret "8f742231b10e8888abcd99yyyzzz85a5"
        timestamp = RequestTimestamp "1531420618"
        signature = RequestSignature "v0=a2114d57b48eac39b9ad189dd8316235a7b4a8d21a10bd27519666489c69b503"
        body = "token=xyzz0WbapA4vBCDEFasx0q6G&team_id=T1DC2JH3J&team_domain=testteamnow&channel_id=G8PSS9T3V&channel_name=foobar&user_id=U2CERLKJA&user_name=roadrunner&command=%2Fwebhook-collect&text=&response_url=https%3A%2F%2Fhooks.slack.com%2Fcommands%2FT1DC2JH3J%2F397700885554%2F96rGlfmibIGlgcZRskXaIFfN&trigger_id=398738663015.47445629121.803a0bc887a14d10d2c447fce8b6703c"
        now = posixSecondsToUTCTime 1531420618
    it "matches Slack's published signing example" $ do
      signRequest secret timestamp body `shouldBe` signature
      verifyRequest 300 now secret timestamp signature body `shouldBe` Right ()
    it "rejects stale requests before comparing signatures" $ do
      verifyRequest 300 (posixSecondsToUTCTime 1531421618) secret timestamp signature body
        `shouldBe` Left TimestampOutsideTolerance

  describe "Events API" $ do
    it "decodes URL verification and preserves unknown envelope types" $ do
      decodeEvent "{\"type\":\"url_verification\",\"challenge\":\"abc\"}"
        `shouldBe` Right (UrlVerification "abc")
      decodeEvent "{\"type\":\"future_type\",\"new_field\":true}"
        `shouldBe` Right (UnknownEventEnvelope "future_type" (object ["type" .= ("future_type" :: Text), "new_field" .= True]))
    it "decodes form-encoded interactivity payloads" $ do
      decodeInteractionPayload "payload=%7B%22type%22%3A%22block_actions%22%7D"
        `shouldBe` Right (object ["type" .= ("block_actions" :: Text)])

  describe "pagination" $ do
    it "follows cursors without duplicating items" $ do
      calls <- newIORef ([] :: [Maybe Cursor])
      let fetch :: Maybe Cursor -> IO (Either Text (Page Int))
          fetch cursor = do
            modifyIORef' calls (<> [cursor])
            pure $ case cursor of
              Nothing -> Right (Page [1, 2] (Just (Cursor "next")))
              Just (Cursor "next") -> Right (Page [3] Nothing)
              Just _ -> Left "unexpected cursor"
      result <- paginate fetch
      result `shouldBe` Right [1, 2, 3]
      readIORef calls `shouldReturnValue` [Nothing, Just (Cursor "next")]
    it "extracts endpoint-specific collections and the standard cursor" $ do
      let value :: Value
          value = object ["channels" .= [object ["id" .= ("C1" :: Text)]], "response_metadata" .= object ["next_cursor" .= ("C2" :: Text)]]
      fmap pageNextCursor (pageFromValue "channels" value :: Either String (Page Value))
        `shouldBe` Right (Just (Cursor "C2"))

  describe "Socket Mode" $ do
    it "builds required acknowledgements with optional response payloads" $ do
      let envelope = SocketEnvelope "env-1" "events_api" (object []) True
      socketAcknowledgement envelope Nothing
        `shouldBe` object ["envelope_id" .= ("env-1" :: Text)]
      socketAcknowledgement envelope (Just (object ["text" .= ("done" :: Text)]))
        `shouldBe` object ["envelope_id" .= ("env-1" :: Text), "payload" .= object ["text" .= ("done" :: Text)]]
      socketAcknowledgement (envelope {socketEnvelopeAcceptsResponsePayload = False}) (Just (object ["text" .= ("ignored" :: Text)]))
        `shouldBe` object ["envelope_id" .= ("env-1" :: Text)]
    it "decodes control messages and delivered envelopes" $ do
      decodeSocketMessage "{\"type\":\"hello\"}" `shouldBe` Right SocketHello
      decodeSocketMessage "{\"type\":\"disconnect\",\"reason\":\"warning\"}" `shouldBe` Right SocketDisconnectWarning
      decodeSocketMessage "{\"type\":\"disconnect\",\"reason\":\"refresh_requested\"}" `shouldBe` Right SocketDisconnect
      decodeSocketMessage "{\"type\":\"events_api\",\"envelope_id\":\"e1\",\"payload\":{},\"accepts_response_payload\":false}"
        `shouldBe` Right (SocketPayload (SocketEnvelope "e1" "events_api" (object []) False))
    it "keeps consuming after warning disconnects" $ do
      messages <- newIORef
        [ "{\"type\":\"disconnect\",\"reason\":\"warning\"}"
        , "{\"type\":\"events_api\",\"envelope_id\":\"e1\",\"payload\":{},\"accepts_response_payload\":false}"
        , "{\"type\":\"disconnect\",\"reason\":\"refresh_requested\"}"
        ]
      handled <- newEmptyMVar
      let receiveMessage = do
            remaining <- readIORef messages
            case remaining of
              next : rest -> writeIORef messages rest >> pure next
              [] -> fail "consumer read past terminal disconnect"
          handler envelope = SocketHandlerResult Nothing (putMVar handled (socketEnvelopeId envelope))
      result <- consumeSocketMessages receiveMessage (const (pure ())) handler
      result `shouldBe` Right ()
      takeMVar handled `shouldReturnValue` "e1"
      readIORef messages `shouldReturnValue` []
    it "acknowledges before starting follow-up work" $ do
      acknowledged <- newIORef False
      observed <- newEmptyMVar
      let envelope = SocketEnvelope "e1" "events_api" (object []) False
          sendAck _ = threadDelay 20000 >> writeIORef acknowledged True
          handler _ = SocketHandlerResult Nothing (readIORef acknowledged >>= putMVar observed)
      dispatchSocketEnvelope sendAck handler envelope
      takeMVar observed `shouldReturnValue` True
    it "returns terminal connection API errors instead of retrying forever" $
      testWithApplication (pure mockSlack) $ \port -> do
        client <- mockClient port
        result <- runSocketMode defaultSocketModeConfig client (token "xapp-invalid") (\_ -> SocketHandlerResult Nothing (pure ()))
        case result of
          Left (SocketModeApiError failure) -> show failure `shouldBe` "Slack API error: invalid_auth"
          Left failure -> expectationFailure (show failure)
          Right () -> expectationFailure "Socket Mode unexpectedly succeeded"
    it "retries server errors and rate limits before returning auth errors" $ do
      attempts <- newIORef (0 :: Int)
      testWithApplication (pure (mockSocketRetries attempts)) $ \port -> do
        client <- mockClient port
        let config = SocketModeConfig 1 1
        result <- runSocketMode config client (token "xapp-invalid") (\_ -> SocketHandlerResult Nothing (pure ()))
        case result of
          Left (SocketModeApiError failure) -> show failure `shouldBe` "Slack API error: invalid_auth"
          Left failure -> expectationFailure (show failure)
          Right () -> expectationFailure "Socket Mode unexpectedly succeeded"
        readIORef attempts `shouldReturnValue` 3

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

isLeft :: Either a b -> Bool
isLeft = not . isRight

shouldReturnValue :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturnValue action expected = action >>= (`shouldBe` expected)

requireMethod :: Text -> IO Method
requireMethod name = either (fail . show) pure (method name)

mockClient :: Int -> IO Client
mockClient port =
  newClientWith
    defaultManagerSettings
    defaultClientSettings
      { clientBaseUrl = "http://127.0.0.1:" <> show port <> "/api/"
      }

mockSlack :: Application
mockSlack request respond = do
  let apiMethod = case pathInfo request of
        ["api", name] -> name
        _ -> "unknown"
      hasBearer = lookup hAuthorization (requestHeaders request) == Just "Bearer xoxb-secret"
      hasJson = lookup hContentType (requestHeaders request) == Just "application/json; charset=utf-8"
  case apiMethod of
    "unknown"
      | pathInfo request == ["hook"] -> respond (responseLBS status200 [] "ok")
    "denied.method" -> respond (responseLBS status200 [(hContentType, "application/json")] "{\"ok\":false,\"error\":\"missing_scope\"}")
    "rate.limited" -> respond (responseLBS status429 [(hRetryAfter, "7")] "slow down")
    "admin.analytics.getFile"
      | lookup hAccept (requestHeaders request) == Just "*/*" ->
          respond (responseLBS status200 [(hContentType, "application/gzip")] "gzip-ndjson-bytes")
      | otherwise -> respond (responseLBS status200 [] "wrong Accept header")
    "warning.method" -> respond (responseLBS status200 [(hContentType, "application/json")] "{\"ok\":true,\"warning\":\"legacy_field,other_warning\",\"warnings\":[\"legacy_field\"],\"response_metadata\":{\"warnings\":[\"other_warning\"]}}")
    "apps.connections.open" -> respond (responseLBS status200 [(hContentType, "application/json")] "{\"ok\":false,\"error\":\"invalid_auth\"}")
    _
      | hasBearer && hasJson ->
          respond
            ( responseLBS
                status200
                [(hContentType, "application/json")]
                ("{\"ok\":true,\"method\":\"" <> LazyByteString.fromStrict (Text.encodeUtf8 apiMethod) <> "\"}")
            )
      | otherwise -> respond (responseLBS status200 [(hContentType, "application/json")] "{\"ok\":false,\"error\":\"bad_transport\"}")

showEither :: Either SlackError value -> String
showEither (Left failure) = show failure
showEither (Right _) = "success"

mockUpload :: IORef [(Text, LazyByteString.ByteString)] -> Application
mockUpload stages request respond = do
  body <- strictRequestBody request
  case pathInfo request of
    ["api", "files.getUploadURLExternal"] -> do
      modifyIORef' stages (<> [("get-url", body)])
      let host = maybe "127.0.0.1" id (requestHeaderHost request)
      respond
        ( responseLBS
            status200
            [(hContentType, "application/json")]
            ("{\"ok\":true,\"upload_url\":\"http://" <> LazyByteString.fromStrict host <> "/upload\",\"file_id\":\"F1\"}")
        )
    ["upload"] -> do
      modifyIORef' stages (<> [("upload", body)])
      respond (responseLBS status200 [] "ok")
    ["api", "files.completeUploadExternal"] -> do
      modifyIORef' stages (<> [("complete", body)])
      respond (responseLBS status200 [(hContentType, "application/json")] "{\"ok\":true,\"files\":[{\"id\":\"F1\",\"title\":\"Report\"}]}")
    _ -> respond (responseLBS status200 [(hContentType, "application/json")] "{\"ok\":false,\"error\":\"unexpected_path\"}")

mockSocketRetries :: IORef Int -> Application
mockSocketRetries attempts request respond = do
  modifyIORef' attempts (+ 1)
  attempt <- readIORef attempts
  case (pathInfo request, attempt) of
    (["api", "apps.connections.open"], 1) ->
      respond (responseLBS status500 [] "temporary failure")
    (["api", "apps.connections.open"], 2) ->
      respond (responseLBS status429 [(hRetryAfter, "0")] "rate limited")
    (["api", "apps.connections.open"], _) ->
      respond (responseLBS status200 [(hContentType, "application/json")] "{\"ok\":false,\"error\":\"invalid_auth\"}")
    _ -> respond (responseLBS status500 [] "unexpected path")
