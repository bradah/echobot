{-# LANGUAGE OverloadedStrings #-}
module Telegram.UpdateParser where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text               as T
import           Telegram.Internal.Types

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
  fmap f (UpdateParser p) =
    UpdateParser $ \upd -> f <$> p upd

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
text = UpdateParser $ updateMessage >=> messageText

-- | Same as 'text' but fails if there wasn't
-- plain text (e.g. command).
plainText :: UpdateParser T.Text
plainText = do
  t <- text
  if "/" `T.isPrefixOf` t
    then empty
    else return t

-- | Check if bot received specific command
command :: T.Text -> UpdateParser ()
command name = do
  t <- text

  case T.words t of
    (w:_) | w == "/" <> name -> pure ()
    _                        -> empty

sticker :: UpdateParser FileId
sticker = UpdateParser $
  updateMessage >=> messageSticker >=> return . stickerFileId

updateChatId :: UpdateParser ChatId
updateChatId = UpdateParser $
  updateMessage >=> return . messageChat >=> return . chatId

updateId :: UpdateParser UpdateId
updateId = UpdateParser (return . updateUpdateId)

callbackQuery :: UpdateParser CallbackQuery
callbackQuery = UpdateParser updateCallbackQuery

callbackId :: UpdateParser CallbackId
callbackId = UpdateParser $
    updateCallbackQuery >=> pure . callbackQueryId

callbackData :: UpdateParser T.Text
callbackData = UpdateParser $
    updateCallbackQuery >=> callbackQueryData

callbackMessageId :: UpdateParser MessageId
callbackMessageId = UpdateParser $
    updateCallbackQuery >=> callbackQueryMessage >=> pure . messageMessageId

callbackChatId :: UpdateParser ChatId
callbackChatId = UpdateParser $
    updateCallbackQuery >=> callbackQueryMessage >=> pure . messageChat
        >=> pure . chatId

updateExample :: Update
updateExample = Update
    { updateUpdateId = 3
    , updateMessage = Just Message
        { messageMessageId = 3
        , messageFrom = Nothing
        , messageDate = 3
        , messageChat = Chat
            { chatId = 3
            , chatUsername = Nothing
            }
        , messageText = Just "/repeat"
        , messageEntities = Nothing
        , messageSticker = Nothing
        , messageReplyMarkup = Nothing
        }
    , updateEditedMessage = Nothing
    , updateCallbackQuery = Nothing
    }
