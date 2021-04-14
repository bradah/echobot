{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines basic Telegram API types.
-}

module Telegram.Data
    ( -- * Available types
      TgState
    , defaultTgState
      -- ** Response
    , Response (..)
    , tryExtract
      -- ** Update
    , Update (..)
      -- ** Message
    , Message (..)
      -- ** User
    , User (..)
      -- ** Chat
    , Chat (..)
      -- ** Message entities
    , MessageEntity (..)
    , MessageEntityType (..)
      -- ** Inline keyboards
    , InlineKeyboardMarkup (..)
    , InlineKeyboardButton (..)
      -- ** Callbacks
    , CallbackQuery (..)
      -- ** Media
    , Sticker (..)
    , PhotoSize (..)
    , Animation (..)
    , Audio (..)
    , Document (..)
    , Video (..)
    , VideoNote (..)
    , Voice (..)
    ) where


import           Control.Monad.Freer
import           Eff.Error
import           Eff.Log

import           AppState              (AppState (..))
import           Data.HashMap.Strict   (fromList)
import           Data.Text             (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           TH                    (deriveFromJSON', deriveJSON')

-- | State in which bot accounts current dialogs.
type TgState = AppState (Maybe Int)

-- | Initial value to start bot with.
defaultTgState :: AppState (Maybe Int)
defaultTgState = AppState
    { st'offset = Nothing
    , st'sessions = fromList []
    }

-- | Telegram response.
data Response a = Response
    { resp'ok          :: Bool
    , resp'description :: Maybe Text
    , resp'result      :: Maybe a
    , resp'errorCode   :: Maybe Int
    } deriving (Show, Eq)

-- | Extract some result from 'Response' or throw an exception.
tryExtract :: ( Members [Error AppError, Log] r
              , Show a
              )
           => Response a
           -> Eff r a
tryExtract resp@(resp'ok -> False) = do
    logError $ "Received error: " <+> resp
    throwError . OtherError $ showT resp
tryExtract (resp'result -> Just r) = do
    logDebug $ "Received response: " <+> r
    pure r
tryExtract resp = do
    logError $ "This can't be possible! " <+> resp
    throwError . OtherError $ showT resp



-- | This object represents an incoming update.
--   At most one of the optional parameters can be
--   present in any given update.
data Update = Update
    { upd'update_id      :: Int
    , upd'message        :: Maybe Message
    , upd'edited_message :: Maybe Message
    , upd'callback_query :: Maybe CallbackQuery
    } deriving (Show, Eq)

-- | This object represents a message.
data Message = Message
    { msg'message_id   :: Int
    , msg'from         :: Maybe User
    , msg'date         :: POSIXTime
    , msg'chat         :: Chat
    , msg'text         :: Maybe Text
    , msg'entities     :: Maybe [MessageEntity]
    , msg'sticker      :: Maybe Sticker
    , msg'reply_markup :: Maybe InlineKeyboardMarkup
    , msg'caption      :: Maybe Text
    , msg'photo        :: Maybe [PhotoSize]
    , msg'animation    :: Maybe Animation
    , msg'audio        :: Maybe Audio
    , msg'document     :: Maybe Document
    , msg'video        :: Maybe Video
    , msg'video_note   :: Maybe VideoNote
    , msg'voice        :: Maybe Voice
    } deriving (Show, Eq)

-- ** User

-- | This object represents a Telegram 'User' or bot.
data User = User
    { usr'id        :: Int
    , usr'is_bot    :: Bool
    , usr'last_name :: Maybe Text
    , usr'username  :: Maybe Text
    } deriving (Show, Eq)

-- | This object represents a chat.
data Chat = Chat
    { chat'id       :: Int
    , chat'username :: Maybe Text
    } deriving (Show, Eq)

-- | This object represents one special entity in a text message.
--   For example, hashtags, usernames, URLs, etc.
data MessageEntity = MessageEntity
    { msgent'type   :: MessageEntityType
    , msgent'offset :: Int
    , msgent'length :: Int
    , msgent'url    :: Maybe Text
    , msgent'user   :: Maybe User
    } deriving (Show, Eq)

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
    deriving (Show, Eq)

-- | This object represents a sticker.
newtype Sticker = Sticker
    { stk'file_id :: Text
    } deriving (Show, Eq)

-- | This object represents an inline keyboard
-- that appears right next to the message it belongs to.
newtype InlineKeyboardMarkup = InlineKeyboardMarkup
    { kbmup'inline_keyboard :: [[InlineKeyboardButton]]
    } deriving (Show, Eq)

-- | This object represents one button of an inline
-- keyboard. You must use exactly one of the optional fields.
data InlineKeyboardButton = InlineKeyboardButton
    { but'text          :: Text
    , but'callback_data :: Maybe Text
    } deriving (Show, Eq)

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
    , cq'from              :: User
    , cq'message           :: Maybe Message
    , cq'inline_message_id :: Maybe Int
    , cq'data              :: Maybe Text
    } deriving (Show, Eq)

-- | This object represents one size of a photo
-- or a file\/sticker thumbnail.
newtype PhotoSize = PhotoSize
    { ph'file_id :: Text
    } deriving (Show, Eq)

-- | This object represents an animation
-- file (GIF or H.264\/MPEG-4 AVC video without sound).
newtype Animation = Animation
    { anim'file_id :: Text
    } deriving (Show, Eq)

-- | This object represents an audio file to be treated as
-- music by the Telegram clients.
newtype Audio = Audio
    { audio'file_id :: Text
    } deriving (Show, Eq)

-- | This object represents a general file
-- (as opposed to photos, voice messages and audio files).
newtype Document = Document
    { doc'file_id :: Text
    } deriving (Show, Eq)

-- | This object represents a video file.
newtype Video = Video
    { vid'file_id :: Text
    } deriving (Show, Eq)

-- | This object represents a video message
-- (available in Telegram apps as of v.4.0).
newtype VideoNote = VideoNote
    { vidnote'file_id :: Text
    } deriving (Show, Eq)

-- | This object represents a voice note.
newtype Voice = Voice
    { voice'file_id :: Text
    } deriving (Show, Eq)

deriveJSON' ''InlineKeyboardButton
deriveJSON' ''InlineKeyboardMarkup
deriveFromJSON' ''Animation
deriveFromJSON' ''Audio
deriveFromJSON' ''Document
deriveFromJSON' ''Video
deriveFromJSON' ''VideoNote
deriveFromJSON' ''Voice
deriveFromJSON' ''PhotoSize
deriveFromJSON' ''CallbackQuery
deriveFromJSON' ''User
deriveFromJSON' ''Chat
deriveFromJSON' ''MessageEntityType
deriveFromJSON' ''MessageEntity
deriveFromJSON' ''Message
deriveFromJSON' ''Update
deriveFromJSON' ''Sticker
deriveFromJSON' ''Response
