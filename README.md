# SLACK

Haskell client for the Slack web api

[![CircleCI](https://circleci.com/gh/owainlewis/slack.svg?style=svg)](https://circleci.com/gh/owainlewis/slack)

## Roadmap

* Web API
* Websocket API
* Stackage Release
* CI

# API Use

All the web API methods are available but you may need to reference the slack docks for parameters.

For a full list of available methods and params visit https://api.slack.com/methods

# Quick Start

```haskell
import qualified Network.Slack.Api as Slack

token :: String
token = "XXX"

-- List channels
--
channels :: IO SlackResponse
channels = Slack.request token "channels.list" []

-- Create a message in the random chat room
--
createExample :: IO SlackResponse
createExample = Slack.request token "chat.postMessage" params
    where params = [("channel", "#random"), ("text", "Hi from Haskell")]

```

Get information about a request endpoint

```haskell
λ> info "channels.list"
"Lists all channels in a Slack team"
```
