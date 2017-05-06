# SLACK

Haskell client for the Slack web api

## Development

```
cabal sandbox init
cabal install --enable-tests
cabal test
```

# Use

All the web api methods are available but you may need to reference the slack docks for parameters.

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
