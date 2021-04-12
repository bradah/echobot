{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Extract data from Vk API 'Update's.
-}

module Vk.Parser
    ( -- * Parse updates
      Parser (..)
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

-- | Get user id from an 'Update'.
updateUserId :: Parser Update Int
updateUserId = Parser $
    pure . upd'object >=> obj'message >=> msg'from_id

-- | Get list of 'Attachment's from an 'Update'.
attachments :: Parser Update [Attachment]
attachments = Parser $
    pure . upd'object >=> obj'message >=> pure . msg'attachments

-- | Get payload from an 'Update'.
payload :: Parser Update T.Text
payload = Parser $
    pure . upd'object >=> obj'message >=> msg'payload

-- | Get sticker id from an 'Update'.
sticker :: Parser Update Int
sticker = do
    atts <- attachments
    case atts of
        [Attachment Sticker Media{..}] -> pure $ fromMaybe 0 media'sticker_id
        _                              -> empty

-- | Check if 'Update' contains audio_message.
isAudioMessage :: Parser Update ()
isAudioMessage = do
    atts <- attachments
    case atts of
        [Attachment AudioMessage _] -> pure ()
        _                           -> empty

-- | Check if 'Update' contains unsupported 'Attachment' type.
unsupported :: Parser Update ()
unsupported = asum
    [ isAudioMessage
    ]
