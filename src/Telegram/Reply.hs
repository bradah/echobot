module Telegram.Reply where

import           Data.Text        (Text)
import           Telegram.Methods
import           Telegram.Request
import           Telegram.Types

replyText :: Token -> ChatId -> Text -> IO (Response (Message))
replyText token cid t =
    sendMessage token $ SendMessageRequest cid t
