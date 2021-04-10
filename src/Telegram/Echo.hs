module Telegram.Echo where

import           Control.Monad.Freer

import           Control.Applicative ((<|>))
import           Control.Monad       (void)
import           Data.Foldable       (asum)
import           Data.Text
import           Echo
import           Log
import           Telegram.Data
import           Telegram.Methods
import           Telegram.Parser

runPureEcho
    :: Method r
    => Eff (Echo Update : r) a
    -> Eff r a
runPureEcho = interpret $ \case
    Listen -> resp'result <$> getUpdates
    Reply u -> case updateToAction u of
        Just act -> act
        Nothing  -> logWarning $ "No matching Action for this Update:" <+> u



updateToAction
    :: Method r
    => Update
    -> Maybe (Eff r ())
updateToAction = runParser . asum $ fmap (fmap void)
    [ sendMessage <$ (command "help" <|> command "start")
            <*> chatId <*> pure helpMessage <*> pure Nothing
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

-- | Greeting message
helpMessage :: Text
helpMessage = Data.Text.unlines
    [ "Hi! I will simply repeat your messages!"
    , "Just imagine talking to a mirror..."
    , ""
    , "Supported messages:"
    , "- plain text"
    , "- stickers"
    , "- photos"
    , "- voice messages"
    , "- video"
    , "- videonotes"
    , "- music"
    , "- documents"
    , "- animation"
    , ""
    , "Supported commands:"
    , "- /help"
    , "- /repeat - ask me to repeat your messages from 1 to 5 times in a row"
    ]
