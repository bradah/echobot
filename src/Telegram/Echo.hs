{- |
Copyright:  (c) 2021 o-pascal
Maintainer: o-pascal <rtrn.0@ya.ru>

Telegram implementation of 'Echo' effect.
-}

module Telegram.Echo
    ( -- * Telegram echo
      -- ** Run
      runPureEcho
    ) where

import           Control.Monad.Freer

import           Control.Applicative ((<|>))
import           Control.Monad       (void)
import           Data.Foldable       (asum)
import           Eff.Echo
import           Eff.Log
import           Telegram.Data
import           Telegram.Methods
import           Telegram.Parser

-- | Run 'Echo'for Telegram purely.
runPureEcho :: Method r
            => Eff (Echo Update : r) a
            -> Eff r a
runPureEcho = interpret $ \case
    Listen -> getUpdates
    Reply u -> case updateToAction u of
        Just act -> act
        Nothing  -> logWarning $ "No matching Action for this Update:" <+> u


-- | Choose proper Telegram API method for a given 'Update'.
-- This makes use of Alternative instance for 'Parser'.
updateToAction :: Method r
               => Update
               -> Maybe (Eff r ())
updateToAction = runParser . asum $ fmap (fmap void)
    [ startCommand <$ (command "help" <|> command "start") <*> chatId
    , repeatCommand <$ command "repeat" <*> chatId
    , sendMessage <$> chatId <*> text <*> pure Nothing
    , sendSticker <$> chatId <*> sticker
    , sendPhoto <$> chatId <*> photo <*> caption
    , sendAnimation <$> chatId <*> animation <*> caption
    , sendAudio <$> chatId <*> audio <*> caption
    , sendDocument <$> chatId <*> document <*> caption
    , sendVideo <$> chatId <*> video <*> caption
    , sendVideoNote <$> chatId <*> videoNote
    , sendVoice <$> chatId <*> voice <*> caption
    ]
        <> fmap (fmap void)
    [ answerRepeatCallback <$> callbackChatId
        <*> callbackMessageId <*> callbackId <*> callbackData
    ]
