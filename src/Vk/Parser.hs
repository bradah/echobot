module Vk.Parser
    ( Parser (..)
    , (<?>)
    , text
    , plainText
    , command
    , updateUserId
    , attachments
    , payload
    , sticker
    , isAudioMessage
    , unsupported
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable       (asum)
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Parser
import           Vk.Data


-- | Extract text from 'Update'.
text :: Parser Update T.Text
text = Parser $
    pure . upd'object >=> obj'message >=> pure . msg'text

-- | Same as 'text' but fails if there wasn't
-- plain text (e.g. command).
plainText :: Parser Update T.Text
plainText = do
  t <- text
  if "/" `T.isPrefixOf` t
    then empty
    else return t

-- | Check if bot received specific command.
command :: T.Text -> Parser Update T.Text
command name = do
  t <- text
  case T.words t of
    (w:ws) | w == "/" <> name -> return (T.unwords ws)
    _                         -> empty

updateUserId :: Parser Update Int
updateUserId = Parser $
    pure . upd'object >=> obj'message >=> msg'from_id

attachments :: Parser Update [Attachment]
attachments = Parser $
    pure . upd'object >=> obj'message >=> pure . msg'attachments

payload :: Parser Update T.Text
payload = Parser $
    pure . upd'object >=> obj'message >=> msg'payload

sticker :: Parser Update Int
sticker = do
    atts <- attachments
    case atts of
        [Attachment Sticker Media{..}] -> pure $ fromMaybe 0 media'sticker_id
        _                              -> empty

isAudioMessage :: Parser Update ()
isAudioMessage = do
    atts <- attachments
    case atts of
        [Attachment AudioMessage _] -> pure ()
        _                           -> empty

unsupported :: Parser Update ()
unsupported = asum
    [ isAudioMessage
    ]

{-
updateChatId :: Parser Update ChatId
updateChatId = Parser $
  updateMessage >=> return . messageChat >=> return . chatId

updateId :: Parser Update UpdateId
updateId = Parser (return . updateUpdateId) -}


