{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Slack's modern external file upload flow.
module Network.Slack.Files
  ( FileUpload (..)
  , UploadedFile (..)
  , uploadFile
  ) where

import Data.Aeson
  ( FromJSON (parseJSON)
  , Value
  , object
  , withObject
  , (.!=)
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)

import Network.Slack.Client (Client)
import Network.Slack.Internal.HTTP (executeExternalUpload)
import Network.Slack.Types
  ( Method
  , SlackError
  , SlackResponse (slackResponseValue)
  , Token
  , method
  )
import Network.Slack.WebAPI (callJSON)

-- | Content and optional sharing details for an external file upload.
data FileUpload = FileUpload
  { fileUploadName :: Text
  , fileUploadContents :: ByteString
  , fileUploadTitle :: Maybe Text
  , fileUploadAltText :: Maybe Text
  , fileUploadSnippetType :: Maybe Text
  , fileUploadChannelId :: Maybe Text
  , fileUploadInitialComment :: Maybe Text
  , fileUploadThreadTimestamp :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

-- | Stable fields returned for a completed upload.
data UploadedFile = UploadedFile
  { uploadedFileId :: Text
  , uploadedFileTitle :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON UploadedFile where
  parseJSON = withObject "uploaded Slack file" $ \objectValue ->
    UploadedFile <$> objectValue .: "id" <*> objectValue .:? "title"

data UploadUrl = UploadUrl
  { uploadUrl :: String
  , uploadFileId :: Text
  }

instance FromJSON UploadUrl where
  parseJSON = withObject "Slack external upload URL" $ \objectValue ->
    UploadUrl <$> objectValue .: "upload_url" <*> objectValue .: "file_id"

newtype CompletedUpload = CompletedUpload {completedFiles :: [UploadedFile]}

instance FromJSON CompletedUpload where
  parseJSON = withObject "Slack completed external upload" $ \objectValue ->
    CompletedUpload <$> objectValue .:? "files" .!= []

-- | Run Slack's current get-URL, upload-bytes, complete-upload sequence.
uploadFile :: Client -> Token -> FileUpload -> IO (Either SlackError [UploadedFile])
uploadFile client authToken upload = do
  uploadTarget <- callJSON client authToken getUploadUrlMethod (uploadUrlRequest upload)
  case uploadTarget of
    Left failure -> pure (Left failure)
    Right response -> do
      let target = slackResponseValue response
      uploaded <- executeExternalUpload client (uploadUrl target) (fileUploadContents upload)
      case uploaded of
        Left failure -> pure (Left failure)
        Right () -> do
          completed <- callJSON client authToken completeUploadMethod (completeRequest upload target)
          pure (fmap (completedFiles . slackResponseValue) completed)

uploadUrlRequest :: FileUpload -> Value
uploadUrlRequest upload =
  object
    ( [ "filename" .= fileUploadName upload
      , "length" .= ByteString.length (fileUploadContents upload)
      ]
        <> catMaybes
          [ ("alt_txt" .=) <$> fileUploadAltText upload
          , ("snippet_type" .=) <$> fileUploadSnippetType upload
          ]
    )

completeRequest :: FileUpload -> UploadUrl -> Value
completeRequest upload target =
  object
    ( [ "files"
          .= [ object
                 ( ["id" .= uploadFileId target]
                     <> maybe [] (\title -> ["title" .= title]) (fileUploadTitle upload)
                 )
             ]
      ]
        <> catMaybes
          [ ("channel_id" .=) <$> fileUploadChannelId upload
          , ("initial_comment" .=) <$> fileUploadInitialComment upload
          , ("thread_ts" .=) <$> fileUploadThreadTimestamp upload
          ]
    )

getUploadUrlMethod :: Method
getUploadUrlMethod = knownMethod "files.getUploadURLExternal"

completeUploadMethod :: Method
completeUploadMethod = knownMethod "files.completeUploadExternal"

knownMethod :: Text -> Method
knownMethod name = case method name of
  Right value -> value
  Left message -> error (show message)
