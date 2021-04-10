{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Vk.Data where

import           AppState              (AppState (..))
import           Control.Applicative   ((<|>))
import           Data.Aeson            (FromJSON (parseJSON), ToJSON (toJSON),
                                        withObject, withText, (.:))
import           Data.HashMap.Strict   (fromList)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text, intercalate, pack)
import           Data.Text.Read        (decimal)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Servant.API           (ToHttpApiData (..))
import           TH                    (deriveFromJSON', snakeFieldModifier)
import           Utils                 (showT)

type VkState = AppState Ts

defaultVkState :: VkState
defaultVkState = AppState
    { st'offset = Ts 0
    , st'sessions = fromList []
    }

newtype Ts = Ts Integer
    deriving (Show, Eq, Num)

instance ToJSON Ts where
    toJSON (Ts ts) = toJSON $ show ts

instance FromJSON Ts where
    parseJSON = withText "Ts" readTs
      where
        readTs = pure . either error (Ts . fst) . decimal

instance ToHttpApiData Ts where
    toUrlPiece (Ts ts) = toUrlPiece ts

data Update = Update
    { upd'type   :: UpdateType
    , upd'object :: Object
    } deriving (Show)

data UpdateType
    = MessageNew
    | MessageReply
    | MessageEdit
    | MessageAllow
    | MessageDeny
    | MessageTypingState
    | MessageEvent
    deriving (Show)

newtype Object = Object
    { obj'message    :: Maybe Message
    } deriving (Show)

data Message = Message
    { msg'id            :: Maybe Int
    , msg'date          :: POSIXTime
    , msg'peer_id       :: Maybe Int
    , msg'from_id       :: Maybe Int
    , msg'text          :: Text
    , msg'attachments   :: [Attachment]
    , msg'payload       :: Maybe Text
    , msg'keyboard      :: Maybe Keyboard
    , msg'fwd_messages  :: Maybe [Message]
    , msg'reply_message :: Maybe Message
    } deriving (Show)

data Attachment = Attachment
    { attachment'type  :: AttachmentType
    , attachment'media :: Media
    } deriving (Show)

instance FromJSON Attachment where
    parseJSON = withObject "attachment" $ \o -> do
        attachment'type <- o .: "type"
        attachment'media <- o .: "photo"
            <|> o .: "video"
            <|> o .: "audio"
            <|> o .: "audio_message"
            <|> o .: "sticker"
            <|> o .: "doc"
            <|> o .: "link"
            <|> o .: "market"
            <|> o .: "wall"
        pure Attachment{..}

instance ToHttpApiData Attachment where
    toUrlPiece (Attachment atType Media{..}) =
        toUrlPiece atType <> case atType of
            Link    -> mempty
            Sticker -> mempty
            Wall    -> maybe "" showT media'from_id
                <> "_"
                <> maybe "" showT media'id
                <> "_"
                <> fromMaybe "" media'access_key
            _       -> maybe "" showT media'owner_id
                <> "_"
                <> maybe "" showT media'id
                <> "_"
                <> fromMaybe "" media'access_key

instance ToHttpApiData [Attachment] where
    toUrlPiece xs = intercalate "," $ toUrlPiece <$> xs

data AttachmentType
    = Photo
    | Video
    | Audio
    | AudioMessage
    | Doc
    | Link
    | Market
    | Wall
    | Sticker
    deriving (Show)

instance ToHttpApiData AttachmentType where
    toUrlPiece = pack . snakeFieldModifier "" . show

data Media = Media
    { media'id          :: Maybe Int
    , media'from_id     :: Maybe Int
    , media'owner_id    :: Maybe Int
    , media'access_key  :: Maybe Text
    , media'attachments :: Maybe [Attachment]
    , media'sticker_id  :: Maybe Int
    , media'url         :: Maybe Text
    , media'title       :: Maybe Text
    , media'caption     :: Maybe Text
    , media'description :: Maybe Text
    } deriving (Show)


data Keyboard = Keyboard
    { keyboard'one_time :: Bool
    , keyboard'button   :: [[Button]]
    , keyboard'inline   :: Bool
    } deriving (Show)

data Button = Button
    { button'action :: ButtonAction
    , button'color  :: ButtonColor
    } deriving (Show)

data ButtonColor
    = Primary
    | Secondary
    | Negative
    | Positive
    deriving (Show)

data ButtonAction = ButtonAction
    { btnAct'type    :: ButtonActionType
    , btnAct'label   :: Maybe Text
    , btnAct'payload :: Text
    } deriving (Show)

data ButtonActionType
    = Text
    deriving (Show)

deriveFromJSON' ''ButtonActionType
deriveFromJSON' ''ButtonAction
deriveFromJSON' ''ButtonColor
deriveFromJSON' ''Button
deriveFromJSON' ''Keyboard
deriveFromJSON' ''Media
deriveFromJSON' ''AttachmentType
deriveFromJSON' ''Message
deriveFromJSON' ''Object
deriveFromJSON' ''UpdateType
deriveFromJSON' ''Update
