module Network.Slack.Websocket where

import qualified Network.Slack.Api as Slack
import Network.Websockets

main = do
    r :: Slack.SlackResponse <- Slack.request token "rtm.start" []
    case r of
        Slack.Success bs -> connectWS bs
        x -> error $ "API response error: " ++ show x

