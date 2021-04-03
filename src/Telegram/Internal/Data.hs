{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines basic Telegram API types.
-}

module Telegram.Internal.Data
    ( -- * Available types
      -- ** Token
      Update(..)
      -- ** Message
    , Message(..)
      -- ** User
    , User(..)
      -- ** Chat
    , Chat(..)
      -- ** Message entities
    , MessageEntity(..)
    , MessageEntityType(..)
      -- ** Inline keyboards
    , InlineKeyboardMarkup(..)
    , InlineKeyboardButton(..)
      -- ** Callbacks
    , CallbackQuery(..)
      -- ** Media
    , Sticker(..)
    , PhotoSize(..)
    , Animation(..)
    , Audio(..)
    , Document(..)
    , Video(..)
    , VideoNote(..)
    , Voice(..)
    ) where


import           Bot.TH
import           Data.Text             (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics


-- | This object represents an incoming update.
--   At most one of the optional parameters can be
--   present in any given update.
data Update = Update
    { upd'update_id      :: Int
    -- ^ The update's unique identifier.
    , upd'message        :: Maybe Message
    -- ^ New incoming message of any kind - text, photo,
    -- sticker, etc.
    , upd'edited_message :: Maybe Message
    -- ^ New version of a message that is known to the bot
    -- and was edited.
    , upd'callback_query :: Maybe CallbackQuery
    -- ^ New incoming callback query.
    } deriving (Show, Generic)

-- | This object represents a message.
data Message = Message
    { msg'message_id   :: Int
    -- ^ Unique message identifier inside this chat.
    , msg'from         :: Maybe User
    -- ^ Sender, empty for messages sent to channels.
    , msg'date         :: POSIXTime
    -- ^ Date the message was sent in Unix time.
    , msg'chat         :: Chat
    -- ^ Conversation the message belongs to.
    , msg'text         :: Maybe Text
    -- ^ For text messages, the actual UTF-8 text
    -- of the message, 0-4096 characters.
    , msg'entities     :: Maybe [MessageEntity]
    -- ^ For text messages, special entities like usernames,
    -- URLs, bot commands, etc. that appear in the text.
    , msg'sticker      :: Maybe Sticker
    -- ^ Message is a sticker, information about the sticker.
    , msg'reply_markup :: Maybe InlineKeyboardMarkup
    -- ^ Inline keyboard attached to the message.
    , msg'caption      :: Maybe Text
    -- ^ Caption for the animation, audio, document,
    -- photo, video or voice, 0-1024 characters.
    , msg'photo        :: Maybe [PhotoSize]
    -- ^ Message is a photo, available sizes of the photo.
    , msg'animation    :: Maybe Animation
    -- ^ Message is an animation, information about the animation.
    -- For backward compatibility, when this field is set,
    -- the document field will also be set.
    , msg'audio        :: Maybe Audio
    -- ^ Message is an audio file, information about the file.
    , msg'document     :: Maybe Document
    -- ^ Message is a general file, information about the file.
    , msg'video        :: Maybe Video
    -- ^ Message is a video, information about the video.
    , msg'video_note   :: Maybe VideoNote
    -- ^ Message is a video note, information about the video message.
    , msg'voice        :: Maybe Voice
    -- ^ Message is a voice message, information about the file.
    } deriving (Show, Generic)

-- ** User

-- | This object represents a Telegram 'User' or bot.
data User = User
    { usr'id         :: Int
    -- ^ Unique identifier for this user or bot.
    , usr'is_bot     :: Bool
    -- ^ True, if this user is a bot.
    , usr'first_name :: Text
    -- ^ User's or bot's first name.
    , usr'last_name  :: Maybe Text
    -- ^ User's or bot's last name.
    , usr'username   :: Maybe Text
    -- ^ User's or bot's username.
    } deriving (Show, Generic)

-- | This object represents a chat.
data Chat = Chat
    { chat'id       :: Int
    -- ^ Unique identifier for this chat.
    , chat'username :: Maybe Text
    -- ^ Username, for private chats, supergroups
    -- and channels if available.
    } deriving (Show, Generic)

-- | This object represents one special entity in a text message.
--   For example, hashtags, usernames, URLs, etc.
data MessageEntity = MessageEntity
    { msgent'type   :: MessageEntityType
    -- ^ Type of the entity. Can be mention (@username),
    --  hashtag, bot_command, url, email, bold
    -- (bold text), italic (italic text),
    -- underline (underlined text), strikethrough,
    -- code (monowidth string), pre (monowidth block),
    -- text_link (for clickable text URLs), text_mention
    -- (for users without usernames).
    , msgent'offset :: Int
    -- ^ Offset in UTF-16 code units to the start
    -- of the entity.
    , msgent'length :: Int
    -- ^ Length of the entity in UTF-16 code units.
    , msgent'url    :: Maybe Text
    -- ^ For “text_link” only, url that will be opened
    -- after user taps on the text.
    , msgent'user   :: Maybe User
    -- ^ For “text_mention” only, the mentioned user.
    } deriving (Show, Generic)

-- | Type of the entity. Can be mention (@username),
--   hashtag, bot_command, url, email, bold (bold text),
--   italic (italic text), underline (underlined text),
--   strikethrough, code (monowidth string),
--   pre (monowidth block), text_link (for clickable text URLs),
--   text_mention (for users without usernames).
data MessageEntityType
    = Mention
    | Hashtag
    | BotCommand
    | Url
    | Email
    | Bold
    | Italic
    | Underline
    | Strikethrough
    | Code
    | Pre
    | TextLink
    | TextMention
    | Cashtag
    | PhoneNumber
    deriving (Show, Generic)

-- | This object represents a sticker.
newtype Sticker = Sticker
    { stk'file_id :: Text
    -- ^ Identifier for this file, which can
    -- be used to download or reuse the file.
    } deriving (Show, Generic)

-- | This object represents an inline keyboard
-- that appears right next to the message it belongs to.
newtype InlineKeyboardMarkup = InlineKeyboardMarkup
    { kbmup'inline_keyboard :: [[InlineKeyboardButton]]
    -- ^ Array of button rows, each represented
    -- by an Array of 'InlineKeyboardButton' objects
    } deriving (Show, Generic)

-- | This object represents one button of an inline
-- keyboard. You must use exactly one of the optional fields.
data InlineKeyboardButton = InlineKeyboardButton
    { but'text          :: Text
    -- ^ Label text on the button
    , but'callback_data :: Maybe Text
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
    { cq'id                :: Text
    -- ^ Unique identifier for this query.
    , cq'from              :: User
    -- ^ Sender.
    , cq'message           :: Maybe Message
    -- ^ Message with the callback button that originated the query.
    -- Note that message content and message date will not
    -- be available if the message is too old.
    , cq'inline_message_id :: Maybe Int
    -- ^ Identifier of the message sent via
    -- the bot in inline mode, that originated the query.
    , cq'data              :: Maybe Text
    -- ^ Data associated with the callback button.
    -- Be aware that a bad client can send arbitrary data in this field.
    } deriving (Show, Generic)

-- | This object represents one size of a photo
-- or a file\/sticker thumbnail.
newtype PhotoSize = PhotoSize
    { ph'file_id :: Text
    } deriving (Show, Generic)

-- | This object represents an animation
-- file (GIF or H.264\/MPEG-4 AVC video without sound).
newtype Animation = Animation
    { anim'file_id :: Text
    } deriving (Show, Generic)

-- | This object represents an audio file to be treated as
-- music by the Telegram clients.
newtype Audio = Audio
    { audio'file_id :: Text
    } deriving (Show, Generic)

-- | This object represents a general file
-- (as opposed to photos, voice messages and audio files).
newtype Document = Document
    { doc'file_id :: Text
    } deriving (Show, Generic)


-- | This object represents a video file.
newtype Video = Video
    { vid'file_id :: Text
    } deriving (Show, Generic)

-- | This object represents a video message
-- (available in Telegram apps as of v.4.0).
newtype VideoNote = VideoNote
    { vidnote'file_id :: Text
    } deriving (Show, Generic)

-- | This object represents a voice note.
newtype Voice = Voice
    { voice'file_id :: Text
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
