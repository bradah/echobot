module Vk.Echo where

import           Control.Monad.Freer

import           Control.Monad       (void)
import           Data.Foldable       (asum)
import           Data.Text

import           Echo
import           Log

import           Vk.Data
import           Vk.Methods
import           Vk.Parser
import           Vk.Requests

runPureEcho
    :: Method r
    => Eff (Echo Update : r) a
    -> Eff r a
runPureEcho = interpret $ \case
    Listen -> checkLpsResp'updates <$> checkLps
    Reply u -> case updateToAction u of
        Just act -> act
        Nothing  -> logWarning $ "No matching Action for this Update:" <+> u


updateToAction
    :: Method r
    => Update
    -> Maybe (Eff r ())
updateToAction = runParser . asum $ fmap (fmap void)
    [ sendMessage <$ command "start" <*> updateUserId <*> pure startMessage <*> pure []
    , sendMessage <$> updateUserId <* unsupported <*> pure unsupportedText <*> pure []
    , sendSticker <$> updateUserId <*> sticker
    , sendMessage <$> updateUserId <*> text <*> attachments
    ]

startMessage :: Text
startMessage = Data.Text.unlines
    [ "Hi! This bot merely echoes your messages c:"
    , ""
    , "Supported messages:"
    , "- plain text"
    , "- stickers"
    , ""
    , "Supported commands:"
    , "- /start"
    ]

unsupportedText :: Text
unsupportedText = "Sorry, VK API doesn't allow me to send messages of this type"
