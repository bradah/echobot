module Telegram.Reply where

import           Data.Text        (Text)
import           Telegram.Methods
import           Telegram.Request
import           Telegram.Types

replyText :: Token -> ChatId -> Text -> IO (Response (Message))
replyText token cid t =
    sendMessage token $ SendMessageRequest cid t

replySticker :: Token -> ChatId -> Sticker -> IO (Response (Message))
replySticker token cid s =
    sendSticker token $ SendStickerRequest cid (stickerFileId s)
