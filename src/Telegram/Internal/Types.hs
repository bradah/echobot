{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines basic Telegram API types.
-}

module Telegram.Internal.Types
    ( -- * Available types
      -- ** Token
      Token
      -- ** Update
    , Update(..)
    , UpdateId
      -- ** Message
    , Message(..)
    , MessageId
    , Caption
      -- ** User
    , User(..)
    , UserId
      -- ** Chat
    , Chat(..)
    , ChatId
      -- ** Message entities
    , MessageEntity(..)
    , MessageEntityType(..)
      -- ** Inline keyboards
    , InlineKeyboardMarkup(..)
    , InlineKeyboardButton(..)
      -- ** Callbacks
    , CallbackQuery(..)
    , CallbackId
      -- ** Media
    , FileId
    , Sticker(..)
    , PhotoSize(..)
    , Animation(..)
    , Audio(..)
    , Document(..)
    , Video(..)
    , VideoNote(..)
    , Voice(..)
    ) where


import           API.TH
import           Data.Int
import           Data.Text             (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics


-- | Bot token.
type Token = Text

-- | This object represents an incoming update.
--   At most one of the optional parameters can be
--   present in any given update.
data Update = Update
  { updateUpdateId      :: UpdateId
  -- ^ The update's unique identifier.
  , updateMessage       :: Maybe Message
  -- ^ New incoming message of any kind - text, photo,
  -- sticker, etc.
  , updateEditedMessage :: Maybe Message
  -- ^ New version of a message that is known to the bot
  -- and was edited.
  , updateCallbackQuery :: Maybe CallbackQuery
  -- ^ New incoming callback query.
  } deriving (Show, Generic)

-- | Unique identifier for 'Update'.
type UpdateId = Int32

-- | This object represents a message.
data Message = Message
    { messageMessageId   :: MessageId
    -- ^ Unique message identifier inside this chat.
    , messageFrom        :: Maybe User
    -- ^ Sender, empty for messages sent to channels.
    , messageDate        :: POSIXTime
    -- ^ Date the message was sent in Unix time.
    , messageChat        :: Chat
    -- ^ Conversation the message belongs to.
    , messageText        :: Maybe Text
    -- ^ For text messages, the actual UTF-8 text
    -- of the message, 0-4096 characters.
    , messageEntities    :: Maybe [MessageEntity]
    -- ^ For text messages, special entities like usernames,
    -- URLs, bot commands, etc. that appear in the text.
    , messageSticker     :: Maybe Sticker
    -- ^ Message is a sticker, information about the sticker.
    , messageReplyMarkup :: Maybe InlineKeyboardMarkup
    -- ^ Inline keyboard attached to the message.
    , messageCaption     :: Caption
    -- ^ Caption for the animation, audio, document,
    -- photo, video or voice, 0-1024 characters.
    , messagePhoto       :: Maybe [PhotoSize]
    -- ^ Message is a photo, available sizes of the photo.
    , messageAnimation   :: Maybe Animation
    -- ^ Message is an animation, information about the animation.
    -- For backward compatibility, when this field is set,
    -- the document field will also be set.
    , messageAudio       :: Maybe Audio
    -- ^ Message is an audio file, information about the file.
    , messageDocument    :: Maybe Document
    -- ^ Message is a general file, information about the file.
    , messageVideo       :: Maybe Video
    -- ^ Message is a video, information about the video.
    , messageVideoNote   :: Maybe VideoNote
    -- ^ Message is a video note, information about the video message.
    , messageVoice       :: Maybe Voice
    -- ^ Message is a voice message, information about the file.
    } deriving (Show, Generic)

-- | Unique identifier for 'Message'.
type MessageId = Int32

-- | Caption for media messages.
type Caption = Maybe Text
-- ** User

-- | This object represents a Telegram 'User' or bot.
data User = User
  { userId        :: UserId
  -- ^ Unique identifier for this user or bot.
  , userIsBot     :: Bool
  -- ^ True, if this user is a bot.
  , userFirstName :: Maybe Text
  -- ^ User's or bot's first name.
  , userLastName  :: Maybe Text
  -- ^ User's or bot's last name.
  , userUsername  :: Maybe Text
  -- ^ User's or bot's username.
  } deriving (Show, Generic)

-- | Unique identifier for this 'User' or bot.
type UserId = Int32

-- | This object represents a chat.
data Chat = Chat
  { chatId       :: ChatId
  -- ^ Unique identifier for this chat.
  , chatUsername :: Maybe Text
  -- ^ Username, for private chats, supergroups
  -- and channels if available.
  } deriving (Show, Generic)

-- | Unique identifier for this chat.
type ChatId = Integer

-- | This object represents one special entity in a text message.
--   For example, hashtags, usernames, URLs, etc.
data MessageEntity = MessageEntity
  { messageEntityType   :: MessageEntityType
  -- ^ Type of the entity. Can be mention (@username),
  --  hashtag, bot_command, url, email, bold
  -- (bold text), italic (italic text),
  -- underline (underlined text), strikethrough,
  -- code (monowidth string), pre (monowidth block),
  -- text_link (for clickable text URLs), text_mention
  -- (for users without usernames).
  , messageEntityOffset :: Int32
  -- ^ Offset in UTF-16 code units to the start
  -- of the entity.
  , messageEntityLength :: Int32
  -- ^ Length of the entity in UTF-16 code units.
  , messageEntityUrl    :: Maybe Text
  -- ^ For “text_link” only, url that will be opened
  -- after user taps on the text.
  , messageEntityUser   :: Maybe User
  -- ^ For “text_mention” only, the mentioned user.
  } deriving (Show, Generic)

-- | Type of the entity. Can be mention (@username),
--   hashtag, bot_command, url, email, bold (bold text),
--   italic (italic text), underline (underlined text),
--   strikethrough, code (monowidth string),
--   pre (monowidth block), text_link (for clickable text URLs),
--   text_mention (for users without usernames).
data MessageEntityType
  = MessageEntityMention
  | MessageEntityHashtag
  | MessageEntityBotCommand
  | MessageEntityUrl
  | MessageEntityEmail
  | MessageEntityBold
  | MessageEntityItalic
  | MessageEntityUnderline
  | MessageEntityStrikethrough
  | MessageEntityCode
  | MessageEntityPre
  | MessageEntityTextLink
  | MessageEntityTextMention
  | MessageEntityCashtag
  | MessageEntityPhoneNumber
  deriving (Show, Generic)

-- | Identifier for this file, which can
-- be used to download or reuse the file.
type FileId = Text

-- | This object represents a sticker.
data Sticker = Sticker
  { stickerFileId :: FileId
  -- ^ Identifier for this file, which can
  -- be used to download or reuse the file.
  } deriving (Show, Generic)

-- | This object represents an inline keyboard
-- that appears right next to the message it belongs to.
newtype InlineKeyboardMarkup = InlineKeyboardMarkup
    { inlineKeyboardMarkupInlineKeyboard :: [[InlineKeyboardButton]]
    -- ^ Array of button rows, each represented
    -- by an Array of 'InlineKeyboardButton' objects
    } deriving (Show, Generic)

-- | This object represents one button of an inline
-- keyboard. You must use exactly one of the optional fields.
data InlineKeyboardButton = InlineKeyboardButton
    { inlineKeyboardButtonText         :: Text
    -- ^ Label text on the button
    , inlineKeyboardButtonCallbackData :: Maybe Text
    -- ^ Data to be sent in a callback
    -- query to the bot when button is pressed,
    -- 1-64 bytes
    } deriving (Show, Generic)

-- | This object represents an incoming callback query
-- from a callback button in an inline keyboard.
-- If the button that originated the query was attached
-- to a message sent by the bot, the field message will
-- be present. If the button was attached to a message
-- sent via the bot (in inline mode), the field
-- inline_message_id will be present. Exactly one of the
-- fields data or game_short_name will be present.
data CallbackQuery = CallbackQuery
    { callbackQueryId              :: CallbackId
    -- ^ Unique identifier for this query.
    , callbackQueryFrom            :: User
    -- ^ Sender.
    , callbackQueryMessage         :: Maybe Message
    -- ^ Message with the callback button that originated the query.
    -- Note that message content and message date will not
    -- be available if the message is too old.
    , callbackQueryInlineMessageId :: Maybe MessageId
    -- ^ Identifier of the message sent via
    -- the bot in inline mode, that originated the query.
    , callbackQueryData            :: Maybe Text
    -- ^ Data associated with the callback button.
    -- Be aware that a bad client can send arbitrary data in this field.
    } deriving (Show, Generic)

-- | Unique identifier for 'CallbackQuery'.
type CallbackId = Text

-- | This object represents one size of a photo
-- or a file\/sticker thumbnail.
data PhotoSize = PhotoSize
    { photoFileId :: FileId
    } deriving (Show, Generic)

-- | This object represents an animation
-- file (GIF or H.264\/MPEG-4 AVC video without sound).
data Animation = Animation
    { animationFileId :: FileId
    } deriving (Show, Generic)

-- | This object represents an audio file to be treated as
-- music by the Telegram clients.
data Audio = Audio
    { audioFileId :: FileId
    } deriving (Show, Generic)

-- | This object represents a general file
-- (as opposed to photos, voice messages and audio files).
data Document = Document
    { documentFileId :: FileId
    } deriving (Show, Generic)


-- | This object represents a video file.
data Video = Video
    { videoFileId :: FileId
    } deriving (Show, Generic)

-- | This object represents a video message
-- (available in Telegram apps as of v.4.0).
data VideoNote = VideoNote
    { videoNoteFileId :: FileId
    } deriving (Show, Generic)

-- | This object represents a voice note.
data Voice = Voice
    { voiceFileId :: FileId
    } deriving (Show, Generic)

deriveJSON' ''Animation
deriveJSON' ''Audio
deriveJSON' ''Document
deriveJSON' ''Video
deriveJSON' ''VideoNote
deriveJSON' ''Voice
deriveJSON' ''PhotoSize
deriveJSON' ''CallbackQuery
deriveJSON' ''InlineKeyboardButton
deriveJSON' ''InlineKeyboardMarkup
deriveJSON' ''User
deriveJSON' ''Chat
deriveJSON' ''MessageEntityType
deriveJSON' ''MessageEntity
deriveJSON' ''Message
deriveJSON' ''Update
deriveJSON' ''Sticker
