{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Vk.Internal.Types where

import           API.TH
import           Data.Int              (Int32)
import           Data.Text             (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           GHC.Generics

type Token = Text

data Update = Update
  { updateType   :: UpdateType
  , updateObject :: Object
  } deriving (Show, Generic)

data UpdateType
  = UpdateTypeMessageNew
  | UpdateTypeMessageReply
  | UpdateTypeMessageEdit
  | UpdateTypeMessageAllow
  | UpdateTypeMessageDeny
  | UpdateTypeMessageTypingState
  | UpdateTypeMessageEvent
  deriving (Show, Generic)

data Object = Object
  { objectMessage    :: Maybe Message
  } deriving (Show, Generic)

data Message = Message
  { messageId           :: MessageId
  , messageDate         :: POSIXTime
  , messagePeerId       :: UserId
  , messageFromId       :: UserId
  , messageText         :: Text
  , messageRandomId     :: RandomId
  , messageAttachments  :: [Attachment]
  , messageImportant    :: Bool
  , messagePayload      :: Maybe Text
  , messageKeyboard     :: Maybe Keyboard
  , messageFwdMessages  :: Maybe [Message]
  , messageReplyMessage :: Maybe Message
  } deriving (Show, Generic)

type RandomId = Int32
type MessageId = Int32

data Attachment = Attachment
  { attachmentType         :: AttachmentType
  , attachmentPhoto        :: Maybe Photo
  , attachmentVideo        :: Maybe Video
  , attachmentAudio        :: Maybe Audio
  , attachmentAudioMessage :: Maybe AudioMessage
  , attachmentDoc          :: Maybe Doc
  , attachmentLink         :: Maybe Link
  , attachmentMarket       :: Maybe Market
  , attachmentMarketAlbum  :: Maybe MarketAlbum
  , attachmentWall         :: Maybe Wall
  , attachmentWallReply    :: Maybe WallReply
  , attachmentSticker      :: Maybe Sticker
  } deriving (Show, Generic)

data AttachmentType
  = AttachmentPhoto
  | AttachmentVideo
  | AttachmentAudio
  | AttachmentAudioMessage
  | AttachmentDoc
  | AttachmentLink
  | AttachmentMarket
  | AttachmentMarketAlbum
  | AttachmentWall
  | AttachmentWallReply
  | AttachmentSticker
  | AttachmentGift
  deriving (Show, Generic)

data Photo = Photo
  { photoId      :: PhotoId
  } deriving (Show, Generic)

type PhotoId = Int32

data Video = Video
  { videoId :: VideoId
  } deriving (Show, Generic)

type VideoId = Int32

data Audio = Audio
  { audioId :: AudioId
  } deriving (Show, Generic)

type AudioId = Int32

data AudioMessage = AudioMessage
  { audioMessageId :: AudioMessageId
  } deriving (Show, Generic)

type AudioMessageId = Int32

data Doc = Doc
  { docId :: DocId
  } deriving (Show, Generic)

type DocId = Int32

data Link = Link
  { linkUrl         :: Text
  , linkTitle       :: Text
  , linkCaption     :: Maybe Text
  , linkDescription :: Text
  , linkPhoto       :: Maybe Photo
  } deriving (Show, Generic)

data Market = Market
  { marketId :: MarketId
  } deriving (Show, Generic)

type MarketId = Int32

data MarketAlbum = MarketAlbum
  { marketAlbumId :: MarketAlbumId
  } deriving (Show, Generic)

type MarketAlbumId = Int32

data Wall = Wall
  { wallId :: WallId
  } deriving (Show, Generic)

type WallId = Int32

data WallReply = WallReply
  { wallReplyId :: WallReplyId
  } deriving (Show, Generic)

type WallReplyId = Int32

data Sticker = Sticker
  { stickerStickerId :: StickerId
  } deriving (Show, Generic)

type StickerId = Int32

data Keyboard = Keyboard
  { keyboardOneTime :: Bool
  , keyboardButton  :: [[Button]]
  , keyboardInline  :: Bool
  } deriving (Show, Generic)

data Button = Button
  { buttonAction :: ButtonAction
  , buttonColor  :: ButtonColor
  } deriving (Show, Generic)

data ButtonColor
  = ButtonColorPrimary
  | ButtonColorSecondary
  | ButtonColorNegative
  | ButtonColorPositive
  deriving (Show, Generic)

data ButtonAction = ButtonAction
  { buttonActionType    :: ButtonActionType
  , buttonActionLabel   :: Maybe Text
  , buttonActionPayload :: Payload
  } deriving (Show, Generic)

data ButtonActionType
  = ButtonActionText
  deriving (Show, Generic)

type Payload = Text

type UserId = Int32


deriveJSON' ''ButtonActionType
deriveJSON' ''ButtonAction
deriveJSON' ''ButtonColor
deriveJSON' ''Button
deriveJSON' ''Keyboard
deriveJSON' ''Sticker
deriveJSON' ''WallReply
deriveJSON' ''Wall
deriveJSON' ''MarketAlbum
deriveJSON' ''Market
deriveJSON' ''Photo
deriveJSON' ''Link
deriveJSON' ''Doc
deriveJSON' ''Audio
deriveJSON' ''AudioMessage
deriveJSON' ''Video
deriveJSON' ''AttachmentType
deriveJSON' ''Attachment
deriveJSON' ''Message
deriveJSON' ''Object
deriveJSON' ''UpdateType
deriveJSON' ''Update
