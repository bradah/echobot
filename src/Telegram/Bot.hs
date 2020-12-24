{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Telegram.Bot where

import           Colog
import           Control.Monad.Reader
import           Data.Configurator
import           Data.Foldable             (asum)
import           Data.Text                 (Text, pack, unlines, unpack)
import           Network.HTTP.Client       (newManager)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Servant.Client

import           API.Bot.Class
import qualified Telegram.Internal.Methods as Internal
import           Telegram.Internal.Request
import qualified Telegram.Internal.Types   as Internal
import           Telegram.UpdateParser

newtype TgBot a = TgBot
    { runTgBot :: ReaderT (Env TgBot) ClientM a
    } deriving (Functor, Applicative, Monad, MonadReader (Env TgBot), MonadIO)

instance HasLog (Env TgBot) Message TgBot where
    getLogAction = envLogAction
    setLogAction act e = e { envLogAction = act }

instance Show (Env TgBot) where
    show (Env token _) = "Env { token = " <> unpack token <> " }"

instance Bot TgBot where
    data Env TgBot = Env
        { envToken :: Internal.Token
        , envLogAction :: LogAction TgBot Message
        }

    type Update TgBot = Internal.Update
    type UpdateId TgBot = Internal.UpdateId

    getUpdates mUid = do
        logInfo "Waiting for updates..."
        ups <- liftClient $ responseResult <$> Internal.getUpdates body
        logInfo $ "Updates received: " <> showT (length ups)
        unless (null ups) $
            logDebug $ "Updates: " <> showT ups
        return ups
      where
        body :: GetUpdatesBody
        body = GetUpdatesBody mUid (Just UpdateMessage) (Just 25)

    getUpdateId ups
        | null ups = pure Nothing
        | otherwise = pure $ (1+) <$> updateId <?> last ups

    mkEnv = do
        Env <$> getToken <*> pure richMessageAction

    handleUpdate up =
        case updateToAction up of
            Just (Start cid)         -> sendText cid startMessage
            Just (EchoText cid t)    -> sendText cid t
            Just (EchoSticker cid s) -> sendSticker cid s
            Nothing                  -> return ()

    unwrap env bot = do
        clientEnv <- defaultClientEnv env
        void $ runClientM (runReaderT (runTgBot bot) env) clientEnv
      where
        defaultClientEnv botEnv = mkClientEnv
            <$> newManager tlsManagerSettings
            <*> pure (botBaseUrl $ envToken botEnv)

data Action
    = Start Internal.ChatId
    | EchoText Internal.ChatId Text
    | EchoSticker Internal.ChatId Internal.FileId

updateToAction :: Update TgBot -> Maybe Action
updateToAction = runUpdateParser $ asum
    [ Start <$ command "start" <*> updateChatId
    , EchoText <$> updateChatId <*> text
    , EchoSticker <$> updateChatId <*> sticker
    ]

liftClient :: ClientM a -> TgBot a
liftClient = TgBot . lift

getToken :: IO Internal.Token
getToken = do
    conf <- load [Required "echobot.conf.local"]
    require conf "telegram.token"

botBaseUrl :: Internal.Token -> BaseUrl
botBaseUrl token = BaseUrl
    Https
    "api.telegram.org"
    443
    (unpack $ "/bot" <> token)

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

showT :: Show a => a -> Text
showT = pack . show

sendText :: Internal.ChatId -> Text -> TgBot ()
sendText cid t = do
    logInfo $ "Sending text \"" <> t <> "\" to chat " <> showT cid
    resp <- liftClient $ Internal.sendMessage body
    logDebug $ "Response: " <> showT resp
  where
    body = SendMessageBody cid t

sendSticker :: Internal.ChatId -> Internal.FileId -> TgBot ()
sendSticker cid fid = do
    logInfo $ "Sending sticker " <> showT fid <> " to chat " <> showT cid
    resp <- liftClient $ Internal.sendSticker body
    logDebug $ "Response: " <> showT resp
  where
    body = SendStickerBody cid fid
