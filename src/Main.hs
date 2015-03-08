{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Slack.Api as Slack

sendMessageExample token channel message =
  Slack.request token "chat.postMessage" [("channel", channel), ("text", message)]

-- sendMessageExample "YOURTOKEN" "#general" "HELLO"
