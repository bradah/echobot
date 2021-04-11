module Vk.Echo where

import           Control.Monad.Freer (Eff, interpret)

import           Control.Monad       (void)
import           Data.Foldable       (asum)
import           Data.Text           (Text)

import           Eff.Echo            (Echo (..))
import           Eff.Log             (logWarning, (<+>))

import           Vk.Data             (Update)
import           Vk.Methods
import           Vk.Parser
import           Vk.Requests         (CheckLpsResponse (checkLpsResp'updates))

runPureEcho
    :: Method r
    => Eff (Echo Update : r) a
    -> Eff r a
runPureEcho = interpret $ \case
    Listen -> checkLpsResp'updates <$> checkLps
    Reply u -> case updateToAction u of
        Just act -> act
        Nothing  -> logWarning $ "No matching Action for this Update: " <+> u


updateToAction
    :: Method r
    => Update
    -> Maybe (Eff r ())
updateToAction = runParser . asum $ fmap (fmap void)
    [ startCommand <$ command "start" <*> updateUserId
    , answerRepeatPayload <$> updateUserId <*> payload
    , repeatCommand <$ command "repeat" <*> updateUserId
    , sendTextWithAttachments <$> updateUserId <* unsupported <*> pure unsupportedText <*> pure []
    , sendSticker <$> updateUserId <*> sticker
    , sendTextWithAttachments <$> updateUserId <*> text <*> attachments
    ]

unsupportedText :: Text
unsupportedText = "Sorry, VK API doesn't allow me to send messages of this type"
