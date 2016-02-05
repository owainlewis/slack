{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- SLACK
--
-- Owain Lewis owain@owainlewis.com
-- A Haskell Client for the Slack Web HTTP API
--
-----------------------------------------------------------------------------

module Main where

import qualified Network.Slack.Api as Slack

sendMessageExample token channel message =
  Slack.request token "chat.postMessage" [("channel", channel), ("text", message)]
