{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Internal.Types where

import           API.Derive
import           Data.Int
import           Data.Text             (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics

-- * Available types

type Token = Text

-- ** Update

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
  } deriving (Show, Generic)

-- | The update's unique identifier.

type UpdateId = Int32


-- ** Message

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
    , messagePhoto       :: Maybe [PhotoSize]
    , messageCaption     :: Maybe Text
    , messageAnimation   :: Animation
    , messageAudio       :: Audio
    , messageDocument    :: Document
    , messageVideo       :: Video
    , messageVideoNote   :: VideoNote
    , messageVoice       :: Voice
    } deriving (Show, Generic)

-- | Unique message identifier.
type MessageId = Int32


-- ** User

-- | This object represents a Telegram user or bot.
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

-- | Unique identifier for this user or bot.
type UserId = Int32


-- ** Chat

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


-- ** MessageEntity

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

-- ** MessageEntityType

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

-- ** Sticker

-- | This object represents a sticker.
data Sticker = Sticker
  { stickerFileId :: FileId
  -- ^ Identifier for this file, which can
  -- be used to download or reuse the file.
  } deriving (Show, Generic)

-- | Identifier for this file, which can
-- be used to download or reuse the file.
type FileId = Text

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
    , callbackQueryFrom            :: User
    , callbackQueryMessage         :: Maybe Message
    , callbackQueryInlineMessageId :: Maybe MessageId
    , callbackQueryData            :: Maybe Text
    } deriving (Show, Generic)

type CallbackId = Text

data PhotoSize = PhotoSize
    { photoFileId :: FileId
    } deriving (Show, Generic)


data Animation = Animation
    { animationFileId :: FileId
    } deriving (Show, Generic)


data Audio = Audio
    { audioFileId :: FileId
    } deriving (Show, Generic)


data Document = Document
    { documentFileId :: FileId
    } deriving (Show, Generic)


data Video = Video
    { videoFileId :: FileId
    } deriving (Show, Generic)


data VideoNote = VideoNote
    { videoNoteFileId :: FileId
    } deriving (Show, Generic)


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
