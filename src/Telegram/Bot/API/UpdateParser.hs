{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.API.UpdateParser where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text              as T
import           Telegram.Bot.API.Types

-- * Parse 'Update's

-- ** UpdateParser

-- | This abstract type represents common record field functions
--   Note that it's essentially the same as 'ReaderT Update Maybe a'
newtype UpdateParser a = UpdateParser
    { runUpdateParser :: Update -> Maybe a }

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

-- | Extract text from 'Update'
text :: UpdateParser T.Text
text = UpdateParser $ updateMessage >=> messageText

-- | Same as 'text' but fails if there wasn't plain text (e.g. command)
plainText :: UpdateParser T.Text
plainText = do
    t <- text
    if "/" `T.isPrefixOf` t
        then empty
        else return t

-- | Check if bot received specific command
command
    :: T.Text -- ^ Expected command (without "/")
    -> UpdateParser T.Text
command name = do
  t <- text
  case T.words t of
    (w:ws) | w == "/" <> name
      -> pure (T.unwords ws)
    _ -> empty

updateMessageText :: Update -> Maybe T.Text
updateMessageText = updateMessage >=> messageText
