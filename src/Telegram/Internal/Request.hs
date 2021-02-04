{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines basic Telegram API types for request bodies
and responses.
-}

module Telegram.Internal.Request
    ( -- * Requests and responses
      -- ** Response
      Response(..)
      -- ** Request bodies
    , GetUpdatesBody(..)
    , UpdateType(..)
    , AnswerCallbackBody(..)
    , EditMessageTextBody(..)
    , SendMessageBody(..)
    , SendStickerBody(..)
    , SendPhotoBody(..)
    , SendAnimationBody(..)
    , SendAudioBody(..)
    , SendDocumentBody(..)
    , SendVideoBody(..)
    , SendVideoNoteBody(..)
    , SendVoiceBody(..)
    ) where


import           Data.Text               (Text)
import           GHC.Generics

import           API.TH
import           Telegram.Internal.Types


-- | Telegram response.
data Response a = Response
  { responseOk          :: Bool
  , responseDescription :: Maybe Text
  , responseResult      :: a
  , responseErrorCode   :: Maybe Int
  } deriving (Show, Generic)

-- | Request body for 'Telegram.Internal.Methods.getUpdates'.
data GetUpdatesBody = GetUpdatesBody
    { getUpdatesOffset     :: Maybe UpdateId
    , getUpdatesUpdateType :: [UpdateType]
    , getUpdatesTimeout    :: Maybe Int
    } deriving (Generic)

-- | Types of updates allowed to receive.
data UpdateType =
  UpdateMessage
  deriving (Generic)

-- | Request body for 'Telegram.Internal.Methods.answerCallbackQuery'.
data AnswerCallbackBody = AnswerCallbackBody
    { answerCallbackCallbackQueryId :: CallbackId
    , answerCallbackText            :: Maybe Text
    , answerCallbackShowAlert       :: Maybe Bool
    } deriving (Generic)

-- | Request body for 'Telegram.Internal.Methods.editMessageText'.
data EditMessageTextBody = EditMessageTextBody
    { editMessageTextChatId      :: ChatId
    , editMessageTextMessageId   :: MessageId
    , editMessageTextText        :: Text
    , editMessageTextReplyMarkup :: Maybe InlineKeyboardMarkup
    } deriving (Generic)

-- | Request body for 'Telegram.Internal.Methods.sendMessage'.
data SendMessageBody = SendMessageBody
  { sendMessageChatId      :: ChatId
  , sendMessageText        :: Text
  , sendMessageReplyMarkup :: Maybe InlineKeyboardMarkup
  } deriving (Generic)

-- | Request body for 'Telegram.Internal.Methods.sendSticker'.
data SendStickerBody = SendStickerBody
  { sendStickerChatId  :: ChatId
  , sendStickerSticker :: FileId
  } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendPhoto'.
data SendPhotoBody = SendPhotoBody
    { sendPhotoChatId  :: ChatId
    , sendPhotoPhoto   :: FileId
    , sendPhotoCaption :: Maybe Text
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendAnimation'.
data SendAnimationBody = SendAnimationBody
    { sendAnimationChatId    :: ChatId
    , sendAnimationAnimation :: FileId
    , sendAnimationCaption   :: Caption
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendAudio'.
data SendAudioBody = SendAudioBody
    { sendAudioChatId  :: ChatId
    , sendAudioAudio   :: FileId
    , sendAudioCaption :: Caption
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendDocument'.
data SendDocumentBody = SendDocumentBody
    { sendDocumentChatId   :: ChatId
    , sendDocumentDocument :: FileId
    , sendDocumentCaption  :: Caption
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendVideo'.
data SendVideoBody = SendVideoBody
    { sendVideoChatId  :: ChatId
    , sendVideoVideo   :: FileId
    , sendVideoCaption :: Caption
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendVideoNote'.
data SendVideoNoteBody = SendVideoNoteBody
    { sendVideoNoteChatId    :: ChatId
    , sendVideoNoteVideoNote :: FileId
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendVoice'.
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
