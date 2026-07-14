{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generic helpers for Slack's cursor pagination convention.
module Network.Slack.Pagination
  ( Cursor (..)
  , Page (..)
  , pageFromValue
  , paginate
  , foldPages
  ) where

import Data.Aeson (FromJSON, Value (Object), parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, withObject, (.:?))
import Data.Text (Text)
import qualified Data.Text as Text

-- | Opaque position supplied by Slack for the next page.
newtype Cursor = Cursor {unCursor :: Text}
  deriving stock (Eq, Show)

-- | One decoded collection page.
data Page a = Page
  { pageItems :: [a]
  , pageNextCursor :: Maybe Cursor
  }
  deriving stock (Eq, Show)

-- | Decode an endpoint-specific collection key and standard response cursor.
pageFromValue :: FromJSON a => Text -> Value -> Either String (Page a)
pageFromValue collectionKey value = parseEither parser value
  where
    parser = withObject "paginated Slack response" $ \object -> do
      itemsValue <- maybe (fail "collection key is missing") pure (KeyMap.lookup (Key.fromText collectionKey) object)
      items <- parseJSON itemsValue
      metadata <- object .:? "response_metadata"
      cursor <- case metadata of
        Just (Object metadataObject) -> metadataObject .:? "next_cursor"
        _ -> pure Nothing
      pure
        Page
          { pageItems = items
          , pageNextCursor = Cursor <$> nonEmpty cursor
          }
    nonEmpty = (>>= \cursorValue -> if Text.null cursorValue then Nothing else Just cursorValue)

-- | Fetch every page, stopping at the first error.
paginate :: Monad m => (Maybe Cursor -> m (Either error (Page item))) -> m (Either error [item])
paginate fetch = fmap (fmap reverse) (foldPages fetch (\items page -> pure (reverse (pageItems page) <> items)) [])

-- | Process pages incrementally without retaining the whole collection.
foldPages
  :: Monad m
  => (Maybe Cursor -> m (Either error (Page item)))
  -> (result -> Page item -> m result)
  -> result
  -> m (Either error result)
foldPages fetch step initial = go Nothing initial
  where
    go cursor result = do
      next <- fetch cursor
      case next of
        Left failure -> pure (Left failure)
        Right page -> do
          updated <- step result page
          case pageNextCursor page of
            Nothing -> pure (Right updated)
            Just nextCursor -> go (Just nextCursor) updated
