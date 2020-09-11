{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Telegram.Request where

import           Data.Aeson
import           Data.Text                    (Text)
import           GHC.Generics
import           Network.HTTP.Req
import           Telegram.Internal.Derivation
import           Telegram.Types

-- * Types representing Telegram response

-- | Bot token
type Token = Text

-- | Telegram response
data Response a = Response
  { responseOk          :: Bool
  , responseDescription :: Maybe Text
  , responseResult      :: a
  , responseErrorCode   :: Maybe Int
  } deriving (Show, Generic)


botBaseUrl :: Text -> Url Https
botBaseUrl token = https "api.telegram.org" /: "bot" <> token

-- | Request body for 'sendMessage'
data SendMessageRequest = SendMessageRequest
  { sendMessageChatId :: ChatId
  , sendMessageText   :: Text
  } deriving (Generic)

-- | Request body for 'getUpdates'
data GetUpdatesRequest = GetUpdatesRequest
  { getUpdatesOffset         :: Maybe UpdateId
  , getUpdaetsAllowedUpdates :: Maybe [UpdateType]
  } deriving (Generic)

-- | Request body for 'sendSticker'
data SendStickerRequest = SendStickerRequest
  { sendStickerChatId  :: ChatId
  , sendStickerSticker :: Text
  } deriving (Generic)

-- | Types of updates allowed to receive
data UpdateType =
  UpdateMessage
  deriving (Generic)

deriveJSON' ''SendMessageRequest
deriveJSON' ''UpdateType
deriveJSON' ''GetUpdatesRequest
deriveJSON' ''Response
deriveJSON' ''SendStickerRequest
