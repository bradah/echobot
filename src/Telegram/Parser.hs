{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Extract data from Telegram API 'Update's.
-}

module Telegram.Parser
    ( -- * Parsing updates
      -- ** Parser
      Parser(..)
    , (<?>)
      -- ** Parse updates
    , chatId
    , updateId
      -- ** Parse callback queries
    , callbackQuery
    , callbackId
    , callbackData
    , callbackMessageId
    , callbackChatId
      -- ** Parse messages
    , text
    , command
    , caption
    , photo
    , audio
    , animation
    , sticker
    , document
    , video
    , videoNote
    , voice
    ) where

import           Control.Applicative
import           Control.Monad       ((>=>))
import           Data.Text           hiding (empty, head)

import           Parser
import           Telegram.Data

-- | Get Chat Id of an 'Update'.
chatId :: Parser Update Int
chatId = Parser $
    upd'message >=> pure . msg'chat >=> pure . chat'id

-- | Get Update Id of an 'Update'.
updateId :: Parser Update Int
updateId = Parser $ pure . upd'update_id

-- | Get CallbackQuery of an 'Update'.
callbackQuery :: Parser Update CallbackQuery
callbackQuery = Parser upd'callback_query

-- | Get Callback Id of an 'Update'.
callbackId :: Parser Update Text
callbackId = Parser $
    upd'callback_query >=> pure . cq'id

-- | Get callback data of an 'Update'.
callbackData :: Parser Update Text
callbackData = Parser $
    upd'callback_query >=> cq'data

-- | Get Message Id from 'CallbackQuery' of an 'Update'.
callbackMessageId :: Parser Update Int
callbackMessageId = Parser $
    upd'callback_query >=> cq'message >=> pure . msg'message_id

-- | Get Chat Id of 'Chat' in which 'CallbackQuery' was issued.
callbackChatId :: Parser Update Int
callbackChatId = Parser $
    upd'callback_query >=> cq'message >=> pure . msg'chat
        >=> pure . chat'id

-- | Get Caption of an 'Update'.
caption :: Parser Update (Maybe Text)
caption = Parser $
    upd'message >=> pure . msg'caption

-- | Extract text from 'Update'.
text :: Parser Update Text
text = Parser $ upd'message >=> msg'text

-- | Check if bot received specific command
command :: Text -> Parser Update ()
command name = do
  t <- text
  case Data.Text.words t of
    (w:_) | w == "/" <> name -> pure ()
    _                        -> empty

-- | Get id of 'Sticker' from an 'Update'.
sticker :: Parser Update Text
sticker = Parser $
  upd'message >=> msg'sticker >=> pure . stk'file_id

-- | Get id of a photo from an 'Update'.
photo :: Parser Update Text
photo = Parser $
    upd'message >=> msg'photo >=> pure . ph'file_id . head

-- | Get id of an animation from an 'Update'.
animation :: Parser Update Text
animation = Parser $
    upd'message >=> msg'animation >=> pure . anim'file_id

-- | Get id of an audio from an 'Update'.
audio :: Parser Update Text
audio = Parser $
    upd'message >=> msg'audio >=> pure . audio'file_id

-- | Get id of a document from an 'Update'.
document :: Parser Update Text
document = Parser $
    upd'message >=> msg'document >=> pure . doc'file_id

-- | Get id of a video from an 'Update'.
video :: Parser Update Text
video = Parser $
    upd'message >=> msg'video >=> pure . vid'file_id

-- | Get id of a video note from an 'Update'.
videoNote :: Parser Update Text
videoNote = Parser $
    upd'message >=> msg'video_note >=> pure . vidnote'file_id

-- | Get id of a voice message from an 'Update'.
voice :: Parser Update Text
voice = Parser $
    upd'message >=> msg'voice >=> pure . voice'file_id
