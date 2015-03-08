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

example = Slack.request token "chat.postMessage" params
    where params = [("channel", "#random"), ("text", "Hi from Haskell")]

 -- Success "{\"ok\":true,\"channel\":\"C03U2KA6Q\" ... "

```
