{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines basic Telegram API types for request bodies
and responses.
-}

module Telegram.Requests
    ( -- * Requests and responses
      -- ** Response
      Response(..)
      -- ** Request bodies
    , GetUpdatesRequest(..)
    , UpdateType(..)
    , AnswerCallbackRequest(..)
    , EditMessageTextRequest(..)
    , SendMessageRequest(..)
    , SendStickerRequest(..)
    , SendPhotoRequest(..)
    , SendAnimationRequest(..)
    , SendAudioRequest(..)
    , SendDocumentRequest(..)
    , SendVideoRequest(..)
    , SendVideoNoteRequest(..)
    , SendVoiceRequest(..)
    ) where


import           Data.Text              (Text)
import           GHC.Generics

import           TH
import           Telegram.Data


-- | Telegram response.
data Response a = Response
    { resp'ok          :: Bool
    , resp'description :: Maybe Text
    , resp'result      :: a
    , resp'errorCode   :: Maybe Int
    } deriving (Show, Generic)

-- | Request body for 'Telegram.Internal.Methods.getUpdates'.
data GetUpdatesRequest = GetUpdatesRequest
    { updates'offset      :: Maybe Int
    , updates'update_type :: [UpdateType]
    , updates'timeout     :: Maybe Int
    } deriving (Generic)

-- | Types of updates allowed to receive.
data UpdateType =
    UpdateMessage
    deriving (Generic)

-- | Request body for 'Telegram.Internal.Methods.answerCallbackQuery'.
data AnswerCallbackRequest = AnswerCallbackRequest
    { callback'callback_query_id :: Text
    , callback'text              :: Maybe Text
    , callback'show_alert        :: Maybe Bool
    } deriving (Generic)

-- | Request body for 'Telegram.Internal.Methods.editMessageText'.
data EditMessageTextRequest = EditMessageTextRequest
    { editMsgText'chat_id      :: Int
    , editMsgText'message_id   :: Int
    , editMsgText'text         :: Text
    , editMsgText'reply_markup :: Maybe InlineKeyboardMarkup
    } deriving (Generic)

-- | Request body for 'Telegram.Internal.Methods.sendMessage'.
data SendMessageRequest = SendMessageRequest
    { sendMsg'chat_id      :: Int
    , sendMsg'text         :: Text
    , sendMsg'reply_markup :: Maybe InlineKeyboardMarkup
    } deriving (Generic)

-- | Request body for 'Telegram.Internal.Methods.sendSticker'.
data SendStickerRequest = SendStickerRequest
    { sendStk'chat_id :: Int
    , sendStk'sticker :: Text
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendPhoto'.
data SendPhotoRequest = SendPhotoRequest
    { sendPh'chat_id :: Int
    , sendPh'photo   :: Text
    , sendPh'caption :: Maybe Text
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendAnimation'.
data SendAnimationRequest = SendAnimationRequest
    { sendAnim'chat_id   :: Int
    , sendAnim'animation :: Text
    , sendAnim'caption   :: Maybe Text
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendAudio'.
data SendAudioRequest = SendAudioRequest
    { sendAudio'chat_id :: Int
    , sendAudio'audio   :: Text
    , sendAudio'caption :: Maybe Text
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendDocument'.
data SendDocumentRequest = SendDocumentRequest
    { sendDoc'chat_id  :: Int
    , sendDoc'document :: Text
    , sendDoc'caption  :: Maybe Text
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendVideo'.
data SendVideoRequest = SendVideoRequest
    { sendVideo'chat_id :: Int
    , sendVideo'video   :: Text
    , sendVideo'caption :: Maybe Text
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendVideoNote'.
data SendVideoNoteRequest = SendVideoNoteRequest
    { sendVideoNote'chat_id   :: Int
    , sendVideoNote'videonote :: Text
    } deriving (Generic)


-- | Request body for 'Telegram.Internal.Methods.sendVoice'.
data SendVoiceRequest = SendVoiceRequest
    { sendVoice'chat_id :: Int
    , sendVoice'voice   :: Text
    , sendVoice'caption :: Maybe Text
    } deriving (Generic)

deriveJSON' ''SendPhotoRequest
deriveJSON' ''EditMessageTextRequest
deriveJSON' ''AnswerCallbackRequest
deriveJSON' ''Response
deriveJSON' ''SendMessageRequest
deriveJSON' ''SendStickerRequest
deriveJSON' ''UpdateType
deriveJSON' ''GetUpdatesRequest
deriveJSON' ''SendAnimationRequest
deriveJSON' ''SendAudioRequest
deriveJSON' ''SendDocumentRequest
deriveJSON' ''SendVideoRequest
deriveJSON' ''SendVideoNoteRequest
deriveJSON' ''SendVoiceRequest
