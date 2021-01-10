{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Internal.Request where

import           API.Derive
import           Data.Text               (Text)
import           GHC.Generics
import           Telegram.Internal.Types

-- * Request bodies

-- | Telegram response.
data Response a = Response
  { responseOk          :: Bool
  , responseDescription :: Maybe Text
  , responseResult      :: a
  , responseErrorCode   :: Maybe Int
  } deriving (Show, Generic)

-- | Request body for 'sendMessage'.
data SendMessageBody = SendMessageBody
  { sendMessageChatId      :: ChatId
  , sendMessageText        :: Text
  , sendMessageReplyMarkup :: Maybe InlineKeyboardMarkup
  } deriving (Generic)

-- | Request body for 'sendSticker'.
data SendStickerBody = SendStickerBody
  { sendStickerChatId  :: ChatId
  , sendStickerSticker :: FileId
  } deriving (Generic)

data GetUpdatesBody = GetUpdatesBody
    { getUpdatesOffset     :: Maybe UpdateId
    , getUpdatesUpdateType :: [UpdateType]
    , getUpdatesTimeout    :: Maybe Int
    } deriving (Generic)

-- | Types of updates allowed to receive.
data UpdateType =
  UpdateMessage
  deriving (Generic)

data AnswerCallbackBody = AnswerCallbackBody
    { answerCallbackCallbackQueryId :: CallbackId
    , answerCallbackText            :: Maybe Text
    , answerCallbackShowAlert       :: Maybe Bool
    } deriving (Generic)

data EditMessageTextBody = EditMessageTextBody
    { editMessageTextBodyChatId      :: ChatId
    , editMessageTextBodyMessageId   :: MessageId
    , editMessageTextBodyText        :: Text
    , editMessageTextBodyReplyMarkup :: Maybe InlineKeyboardMarkup
    } deriving (Generic)

data SendPhotoBody = SendPhotoBody
    { sendPhotoBodyChatId  :: ChatId
    , sendPhotoBodyPhoto   :: FileId
    , sendPhotoBodyCaption :: Maybe Text
    } deriving (Generic)

deriveJSON' ''SendPhotoBody
deriveJSON' ''EditMessageTextBody
deriveJSON' ''AnswerCallbackBody
deriveJSON' ''Response
deriveJSON' ''SendMessageBody
deriveJSON' ''SendStickerBody
deriveJSON' ''UpdateType
deriveJSON' ''GetUpdatesBody
