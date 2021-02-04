{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module contains lens-like functions to extract fields from
Telegram API ADTs such as 'Update' or 'Message'.
-}

module Telegram.UpdateParser
    ( -- * Parsing updates
      -- ** UpdateParser
      UpdateParser(..)
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

import           Telegram.Internal.Types

-- | Generalized type of record field selectors.
newtype UpdateParser a = UpdateParser
  { runUpdateParser :: Update -> Maybe a }

-- | Infix version of 'runUpdateParser'.
infixl 4 <?>
(<?>) :: UpdateParser a -> Update -> Maybe a
(<?>) = runUpdateParser

instance Functor UpdateParser where
  fmap f (UpdateParser p) =
    UpdateParser $ \upd -> f <$> p upd

instance Applicative UpdateParser where
  pure = UpdateParser . pure . pure
  UpdateParser f <*> UpdateParser x =
    UpdateParser $ \upd -> f upd <*> x upd

instance Monad UpdateParser where
  return = pure
  UpdateParser x >>= f =
    UpdateParser $ \upd -> x upd >>= flip runUpdateParser upd . f

instance Alternative UpdateParser where
  empty = UpdateParser (const Nothing)
  UpdateParser f <|> UpdateParser g = UpdateParser (\u -> f u <|> g u)

-- | Get 'ChatId' of an 'Update'.
updateChatId :: UpdateParser ChatId
updateChatId = UpdateParser $
  updateMessage >=> return . messageChat >=> return . chatId

-- | Get 'UpdateId' of an 'Update'.
updateId :: UpdateParser UpdateId
updateId = UpdateParser (return . updateUpdateId)

-- | Get 'CallbackQuery' of an 'Update'.
callbackQuery :: UpdateParser CallbackQuery
callbackQuery = UpdateParser updateCallbackQuery

-- | Get 'CallbackId' of an 'Update'.
callbackId :: UpdateParser CallbackId
callbackId = UpdateParser $
    updateCallbackQuery >=> pure . callbackQueryId

-- | Get callback data of an 'Update'.
callbackData :: UpdateParser T.Text
callbackData = UpdateParser $
    updateCallbackQuery >=> callbackQueryData

-- | Get 'MessageId' from 'CallbackQuery' of an 'Update'.
callbackMessageId :: UpdateParser MessageId
callbackMessageId = UpdateParser $
    updateCallbackQuery >=> callbackQueryMessage >=> pure . messageMessageId

-- | Get 'ChatId' of 'Chat' in which 'CallbackQuery' was issued.
callbackChatId :: UpdateParser ChatId
callbackChatId = UpdateParser $
    updateCallbackQuery >=> callbackQueryMessage >=> pure . messageChat
        >=> pure . chatId

-- | Get 'Caption' of an 'Update'.
caption :: UpdateParser Caption
caption = UpdateParser $
    updateMessage >=> pure . messageCaption

-- | Extract text from 'Update'.
text :: UpdateParser T.Text
text = UpdateParser $ updateMessage >=> messageText

-- | Same as 'text' but fails if there wasn't
-- plain text (e.g. command).
plainText :: UpdateParser T.Text
plainText = do
  t <- text
  if "/" `T.isPrefixOf` t
    then empty
    else return t

-- | Check if bot received specific command
command :: T.Text -> UpdateParser ()
command name = do
  t <- text

  case T.words t of
    (w:_) | w == "/" <> name -> pure ()
    _                        -> empty

-- | Get 'Sticker' from an 'Update'.
sticker :: UpdateParser FileId
sticker = UpdateParser $
  updateMessage >=> messageSticker >=> return . stickerFileId

-- | Get 'FileId' of a photo from an 'Update'.
photo :: UpdateParser FileId
photo = UpdateParser $
    updateMessage >=> messagePhoto >=> pure . photoFileId . head

-- | Get 'FileId' of an animation from an 'Update'.
animation :: UpdateParser FileId
animation = UpdateParser $
    updateMessage >=> messageAnimation >=> pure . animationFileId

-- | Get 'FileId' of an audio from an 'Update'.
audio :: UpdateParser FileId
audio = UpdateParser $
    updateMessage >=> messageAudio >=> pure . audioFileId

-- | Get 'FileId' of a document from an 'Update'.
document :: UpdateParser FileId
document = UpdateParser $
    updateMessage >=> messageDocument >=> pure . documentFileId

-- | Get 'FileId' of a video from an 'Update'.
video :: UpdateParser FileId
video = UpdateParser $
    updateMessage >=> messageVideo >=> pure . videoFileId

-- | Get 'FileId' of a video note from an 'Update'.
videoNote :: UpdateParser FileId
videoNote = UpdateParser $
    updateMessage >=> messageVideoNote >=> pure . videoNoteFileId

-- | Get 'FileId' of a voice message from an 'Update'.
voice :: UpdateParser FileId
voice = UpdateParser $
    updateMessage >=> messageVoice >=> pure . voiceFileId
