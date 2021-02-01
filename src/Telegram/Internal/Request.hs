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
    { editMessageTextChatId      :: ChatId
    , editMessageTextMessageId   :: MessageId
    , editMessageTextText        :: Text
    , editMessageTextReplyMarkup :: Maybe InlineKeyboardMarkup
    } deriving (Generic)

data SendPhotoBody = SendPhotoBody
    { sendPhotoChatId  :: ChatId
    , sendPhotoPhoto   :: FileId
    , sendPhotoCaption :: Maybe Text
    } deriving (Generic)


data SendAnimationBody = SendAnimationBody
    { sendAnimationChatId    :: ChatId
    , sendAnimationAnimation :: FileId
    , sendAnimationCaption   :: Caption
    } deriving (Generic)


data SendAudioBody = SendAudioBody
    { sendAudioChatId  :: ChatId
    , sendAudioAudio   :: FileId
    , sendAudioCaption :: Caption
    } deriving (Generic)


data SendDocumentBody = SendDocumentBody
    { sendDocumentChatId   :: ChatId
    , sendDocumentDocument :: FileId
    , sendDocumentCaption  :: Caption
    } deriving (Generic)


data SendVideoBody = SendVideoBody
    { sendVideoChatId  :: ChatId
    , sendVideoVideo   :: FileId
    , sendVideoCaption :: Caption
    } deriving (Generic)


data SendVideoNoteBody = SendVideoNoteBody
    { sendVideoNoteChatId    :: ChatId
    , sendVideoNoteVideoNote :: FileId
    } deriving (Generic)


data SendVoiceBody = SendVoiceBody
    { sendVoiceChatId  :: ChatId
    , sendVoiceVoice   :: FileId
    , sendVoiceCaption :: Caption
    } deriving (Generic)

deriveJSON' ''SendPhotoBody
deriveJSON' ''EditMessageTextBody
deriveJSON' ''AnswerCallbackBody
deriveJSON' ''Response
deriveJSON' ''SendMessageBody
deriveJSON' ''SendStickerBody
deriveJSON' ''UpdateType
deriveJSON' ''GetUpdatesBody
deriveJSON' ''SendAnimationBody
deriveJSON' ''SendAudioBody
deriveJSON' ''SendDocumentBody
deriveJSON' ''SendVideoBody
deriveJSON' ''SendVideoNoteBody
deriveJSON' ''SendVoiceBody
