# slack-hs

A modern Haskell SDK for the Slack platform.

`slack-hs` covers the complete Slack Web API without maintaining a brittle list
of endpoint wrappers. A validated method name and any `ToJSON` request can call
every current method, including `admin.*`, `apps.*`, `conversations.*`, and new
methods Slack adds later. Responses can be decoded into `Value` or your own
`FromJSON` types.

The package also supports:

- bearer authentication and JSON, form, or query parameters;
- typed Slack, HTTP, decoding, transport, and rate-limit errors;
- response warnings and metadata;
- cursor pagination;
- the current external file upload flow;
- Events API envelopes and signed request verification;
- interactive payload decoding;
- Socket Mode open, control messages, acknowledgements, and reconnects;
- incoming webhooks;
- a generic `slack-api` command.

It supports GHC 9.6 and newer.

## Web API

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, Value, object, (.=))
import GHC.Generics (Generic)
import Network.Slack

data Conversations = Conversations
  { channels :: [Value]
  }
  deriving (Generic, Show)

instance FromJSON Conversations

main :: IO ()
main = do
  client <- newClient
  apiMethod <- either (fail . show) pure (method "conversations.list")
  result <-
    callJSON
      client
      (token "xoxb-your-token")
      apiMethod
      (object ["limit" .= (200 :: Int)])
  case result of
    Left failure -> print failure
    Right response -> print (channels (slackResponseValue response))
```

`call` accepts a `SlackRequest` when a method needs GET parameters, form data,
or no bearer token. `callForm` and `callUnauthenticatedJSON` cover common cases.
Tokens and signing secrets have redacted `Show` instances.

### Errors and rate limits

No API failure is thrown as an exception. Match on `SlackError`:

```haskell
case result of
  Left (RateLimited retryAfterSeconds _) ->
    putStrLn ("retry after " <> show retryAfterSeconds <> " seconds")
  Left (ApiError slackFailure) ->
    print (slackApiErrorCode slackFailure)
  Left failure -> print failure
  Right response -> print (slackResponseValue response)
```

Transport exceptions are captured as `TransportError`. Asynchronous exceptions
are not used for normal Web API results.

### Streaming responses

Most Web API methods return JSON envelopes. Methods such as
`admin.analytics.getFile` return large gzip streams instead. Use
`withRawResponse` for these methods; its callback receives status, headers, and
an `http-client` `BodyReader` and must consume the reader before returning.

### Pagination

Slack collection keys vary by method, so pagination keeps fetching separate
from page decoding:

```haskell
allItems <- paginate $ \cursor -> do
  response <- fetchOnePage cursor
  pure $ response >>= pageFromValue "channels" . slackResponseValue
```

Use `foldPages` to process large collections without retaining every item.

## Files

`uploadFile` implements Slack's three-step external upload sequence:
`files.getUploadURLExternal`, binary upload, then
`files.completeUploadExternal`.

```haskell
result <- uploadFile client authToken FileUpload
  { fileUploadName = "report.txt"
  , fileUploadContents = "hello"
  , fileUploadTitle = Just "Report"
  , fileUploadAltText = Nothing
  , fileUploadSnippetType = Just "text"
  , fileUploadChannelId = Just "C123"
  , fileUploadInitialComment = Nothing
  , fileUploadThreadTimestamp = Nothing
  }
```

## Events and interactivity

Verify the raw request body before decoding it. Slack recommends rejecting
timestamps more than five minutes from the local clock.

```haskell
verification =
  verifyRequest
    300
    now
    (signingSecret signingSecretBytes)
    (RequestTimestamp timestampHeader)
    (RequestSignature signatureHeader)
    rawBody
```

After verification, use `decodeEvent` for Events API JSON or
`decodeInteractionPayload` for form-encoded shortcuts, actions, and views.
Unknown event envelope types are preserved as raw JSON for forward
compatibility.

## Socket Mode

```haskell
runSocketMode defaultSocketModeConfig client (token "xapp-your-app-token") $ \envelope -> do
  SocketHandlerResult
    { socketResponsePayload = Nothing
    , socketAfterAcknowledgement = handleEnvelope envelope
    }
```

The client obtains a temporary URL with `apps.connections.open`, ignores
`hello`, handles `disconnect`, acknowledges each envelope before starting user
work, and opens a fresh connection after disconnects. Put any immediate response
in `socketResponsePayload` only when Slack accepts one. Terminal API errors are
returned; transient connection failures use jittered exponential backoff.

## Incoming webhooks

```haskell
postWebhook client webhookUrl (object ["text" .= ("deployed" :: Text)])
```

## CLI

```console
SLACK_TOKEN="$SLACK_BOT_TOKEN" cabal run slack-api -- \
  conversations.list \
  --json '{"limit":200}'
```

## Migrating from 0.1

The 1.0 API is intentionally not source compatible with the 2017 package.

- Replace endpoint functions such as `channelsList` with `method` and
  `callJSON` or `callForm`.
- Replace `String` tokens with `Token` values created by `token`.
- Replace raw lazy `ByteString` results with `SlackResponse a`.
- Handle `SlackError`, especially `ApiError` and `RateLimited`.
- Replace RTM with Socket Mode. Slack's current flow starts with
  `apps.connections.open`.
- Replace `files.upload` with `uploadFile`.

## Development

```console
cabal test all
cabal haddock all
cabal sdist
```

The tests use an in-process mock Slack server. They do not require credentials
or network access.

Slack references: [Web API](https://docs.slack.dev/apis/web-api/),
[pagination](https://docs.slack.dev/apis/web-api/pagination),
[rate limits](https://docs.slack.dev/apis/web-api/rate-limits/),
[request signing](https://docs.slack.dev/authentication/verifying-requests-from-slack/),
and [Socket Mode](https://docs.slack.dev/apis/events-api/using-socket-mode/).
