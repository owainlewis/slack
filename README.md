# SLACK

Haskell client for the Slack web api

# Use

All the web api methods are available but you may need to reference the slack docks for parameters.

For a full list of available methods and params visit https://api.slack.com/methods

# Quick Start


```haskell
import qualified Network.Slack.Api as Slack

token = "YOURTOKENHERE"

-- Let's create a message in the random chat room

example = Slack.request token "chat.postMessage" [("channel", "#random"), ("text", "Hi from Haskell")]

 -- Success "{\"ok\":true,\"channel\":\"C03U2KA6Q\",\"ts\":\"1425844230.000002\",\"message\":{\"text\":\"Hi from Haskell\",\"username\":\"bot\",\"type\":\"message\",\"subtype\":\"bot_message\",\"ts\":\"1425844230.000002\"}}"

```
