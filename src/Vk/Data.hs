{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Vk.Data where

import           Control.Applicative   ((<|>))
import           Data.Aeson            (FromJSON (parseJSON), ToJSON (toJSON),
                                        withObject, withText, (.:))
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text, intercalate, pack)
import           Data.Text.Read        (decimal)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Servant.API           (ToHttpApiData (..))
import           TH                    (deriveFromJSON', snakeFieldModifier)
import           Utils                 (showT)

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

type Token = Text

data Update = Update
  { updateType   :: UpdateType
  , updateObject :: Object
  } deriving (Show)

data UpdateType
  = UpdateTypeMessageNew
  | UpdateTypeMessageReply
  | UpdateTypeMessageEdit
  | UpdateTypeMessageAllow
  | UpdateTypeMessageDeny
  | UpdateTypeMessageTypingState
  | UpdateTypeMessageEvent
  deriving (Show)

data Object = Object
  { objectMessage    :: Maybe Message
  } deriving (Show)

data Message = Message
  { messageId           :: Maybe MessageId
  , messageDate         :: POSIXTime
  , messagePeerId       :: Maybe UserId
  , messageFromId       :: Maybe UserId
  , messageText         :: Text
  , messageAttachments  :: [Attachment]
  , messagePayload      :: Maybe Text
  , messageKeyboard     :: Maybe Keyboard
  , messageFwdMessages  :: Maybe [Message]
  , messageReplyMessage :: Maybe Message
  } deriving (Show)

type RandomId = Int
type MessageId = Int

data Attachment = Attachment
  { attachmentType  :: AttachmentType
  , attachmentMedia :: Media
  } deriving (Show)

instance FromJSON Attachment where
    parseJSON = withObject "attachment" $ \o -> do
        attachmentType <- o .: "type"
        attachmentMedia <- o .: "photo"
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
            Wall    -> maybe "" showT mediaFromId
                <> "_"
                <> maybe "" showT mediaId
                <> "_"
                <> fromMaybe "" mediaAccessKey
            _       -> maybe "" showT mediaOwnerId
                <> "_"
                <> maybe "" showT mediaId
                <> "_"
                <> fromMaybe "" mediaAccessKey

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
    { mediaId          :: Maybe MediaId
    , mediaFromId      :: Maybe UserId
    , mediaOwnerId     :: Maybe UserId
    , mediaAccessKey   :: Maybe Text
    , mediaAttachments :: Maybe [Attachment]
    , mediaStickerId   :: Maybe StickerId
    , mediaUrl         :: Maybe Text
    , mediaTitle       :: Maybe Text
    , mediaCaption     :: Maybe Text
    , mediaDescription :: Maybe Text
    } deriving (Show)

type MediaId = Int
type StickerId = Int

data Keyboard = Keyboard
  { keyboardOneTime :: Bool
  , keyboardButton  :: [[Button]]
  , keyboardInline  :: Bool
  } deriving (Show)

data Button = Button
  { buttonAction :: ButtonAction
  , buttonColor  :: ButtonColor
  } deriving (Show)

data ButtonColor
  = ButtonColorPrimary
  | ButtonColorSecondary
  | ButtonColorNegative
  | ButtonColorPositive
  deriving (Show)

data ButtonAction = ButtonAction
  { buttonActionType    :: ButtonActionType
  , buttonActionLabel   :: Maybe Text
  , buttonActionPayload :: Payload
  } deriving (Show)

data ButtonActionType
  = ButtonActionText
  deriving (Show)

type Payload = Text

type UserId = Int

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
