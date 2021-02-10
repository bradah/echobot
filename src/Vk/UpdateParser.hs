{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Vk.UpdateParser where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable       (asum)
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Vk.Internal.Types

-- * Parse 'Update's

-- ** UpdateParser

-- | Generalized type of record field selectors.
newtype UpdateParser a = UpdateParser
  { runUpdateParser :: Update -> Maybe a }


-- | Infix version of 'runUpdateParser'.
infixl 4 <?>
(<?>) :: UpdateParser a -> Update -> Maybe a
(<?>) = runUpdateParser

instance Functor UpdateParser where
  fmap f (UpdateParser p) = UpdateParser $ fmap f . p

instance Applicative UpdateParser where
  pure = UpdateParser . pure . pure
  UpdateParser f <*> UpdateParser x =
    UpdateParser $ \upd -> f upd <*> x upd

instance Monad UpdateParser where
  return = pure
  UpdateParser x >>= f =
    UpdateParser $ \upd -> x upd >>= flip runUpdateParser upd . f

instance Alternative UpdateParser where
  empty = UpdateParser (const Nothing)
  UpdateParser f <|> UpdateParser g = UpdateParser (\u -> f u <|> g u)


-- ** Some useful parsers

-- | Extract text from 'Update'.
text :: UpdateParser T.Text
text = UpdateParser $
    pure . updateObject >=> objectMessage >=> pure . messageText

-- | Same as 'text' but fails if there wasn't
-- plain text (e.g. command).
plainText :: UpdateParser T.Text
plainText = do
  t <- text
  if "/" `T.isPrefixOf` t
    then empty
    else return t

-- | Check if bot received specific command.
command :: T.Text -> UpdateParser T.Text
command name = do
  t <- text
  case T.words t of
    (w:ws) | w == "/" <> name -> return (T.unwords ws)
    _                         -> empty

updateUserId :: UpdateParser UserId
updateUserId = UpdateParser $
    pure . updateObject >=> objectMessage >=> messageFromId

attachments :: UpdateParser [Attachment]
attachments = UpdateParser $
    pure . updateObject >=> objectMessage >=> pure . messageAttachments

sticker :: UpdateParser StickerId
sticker = do
    atts <- attachments
    case atts of
        [Attachment Sticker Media{..}] -> pure $ fromMaybe 0 mediaStickerId
        _                              -> empty

isAudioMessage :: UpdateParser ()
isAudioMessage = do
    atts <- attachments
    case atts of
        [Attachment AudioMessage _] -> pure ()
        _                           -> empty

unsupported :: UpdateParser ()
unsupported = asum
    [ isAudioMessage
    ]

{-
updateChatId :: UpdateParser ChatId
updateChatId = UpdateParser $
  updateMessage >=> return . messageChat >=> return . chatId

updateId :: UpdateParser UpdateId
updateId = UpdateParser (return . updateUpdateId) -}


