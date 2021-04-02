{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module contains lens-like functions to extract fields from
Telegram API ADTs such as 'Update' or 'Message'.
-}

module Telegram.Parser
    ( -- * Parsing updates
      -- ** Parser
      Parser(..)
    , (<?>)
      -- ** Parse updates
    , updateChatId
    , updateId
      -- ** Parse callback queries
    , callbackQuery
    , callbackId
    , callbackData
    , callbackMessageId
    , callbackChatId
      -- ** Parse messages
    , plainText
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
import           Control.Monad
import qualified Data.Text               as T

import           Bot.Parser
import           Telegram.Internal.Types


-- | Get 'ChatId' of an 'Update'.
updateChatId :: Parser Update ChatId
updateChatId = Parser $
  updateMessage >=> pure . messageChat >=> pure . chatId

-- | Get 'UpdateId' of an 'Update'.
updateId :: Parser Update UpdateId
updateId = Parser (pure . updateUpdateId)

-- | Get 'CallbackQuery' of an 'Update'.
callbackQuery :: Parser Update CallbackQuery
callbackQuery = Parser updateCallbackQuery

-- | Get 'CallbackId' of an 'Update'.
callbackId :: Parser Update CallbackId
callbackId = Parser $
    updateCallbackQuery >=> pure . callbackQueryId

-- | Get callback data of an 'Update'.
callbackData :: Parser Update T.Text
callbackData = Parser $
    updateCallbackQuery >=> callbackQueryData

-- | Get 'MessageId' from 'CallbackQuery' of an 'Update'.
callbackMessageId :: Parser Update MessageId
callbackMessageId = Parser $
    updateCallbackQuery >=> callbackQueryMessage >=> pure . messageMessageId

-- | Get 'ChatId' of 'Chat' in which 'CallbackQuery' was issued.
callbackChatId :: Parser Update ChatId
callbackChatId = Parser $
    updateCallbackQuery >=> callbackQueryMessage >=> pure . messageChat
        >=> pure . chatId

-- | Get 'Caption' of an 'Update'.
caption :: Parser Update Caption
caption = Parser $
    updateMessage >=> pure . messageCaption

-- | Extract text from 'Update'.
text :: Parser Update T.Text
text = Parser $ updateMessage >=> messageText

-- | Same as 'text' but fails if there wasn't
-- plain text (e.g. command).
plainText :: Parser Update T.Text
plainText = do
  t <- text
  if "/" `T.isPrefixOf` t
    then empty
    else pure t

-- | Check if bot received specific command
command :: T.Text -> Parser Update ()
command name = do
  t <- text

  case T.words t of
    (w:_) | w == "/" <> name -> pure ()
    _                        -> empty

-- | Get 'Sticker' from an 'Update'.
sticker :: Parser Update FileId
sticker = Parser $
  updateMessage >=> messageSticker >=> pure . stickerFileId

-- | Get 'FileId' of a photo from an 'Update'.
photo :: Parser Update FileId
photo = Parser $
    updateMessage >=> messagePhoto >=> pure . photoFileId . head

-- | Get 'FileId' of an animation from an 'Update'.
animation :: Parser Update FileId
animation = Parser $
    updateMessage >=> messageAnimation >=> pure . animationFileId

-- | Get 'FileId' of an audio from an 'Update'.
audio :: Parser Update FileId
audio = Parser $
    updateMessage >=> messageAudio >=> pure . audioFileId

-- | Get 'FileId' of a document from an 'Update'.
document :: Parser Update FileId
document = Parser $
    updateMessage >=> messageDocument >=> pure . documentFileId

-- | Get 'FileId' of a video from an 'Update'.
video :: Parser Update FileId
video = Parser $
    updateMessage >=> messageVideo >=> pure . videoFileId

-- | Get 'FileId' of a video note from an 'Update'.
videoNote :: Parser Update FileId
videoNote = Parser $
    updateMessage >=> messageVideoNote >=> pure . videoNoteFileId

-- | Get 'FileId' of a voice message from an 'Update'.
voice :: Parser Update FileId
voice = Parser $
    updateMessage >=> messageVoice >=> pure . voiceFileId
