{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Telegram.Bot where

import           API.Bot.Class
import           Control.Monad.Reader
import           Data.Configurator
import           Data.Foldable            (asum)
import           Data.Text                (Text, unlines, unpack)
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Servant.Client
import qualified Telegram.Methods         as Tg
import qualified Telegram.Methods.Request as Tg
import qualified Telegram.Types           as Tg
import           Telegram.UpdateParser

newtype TgBot a = TgBot
  { runTgBot :: ReaderT (Env TgBot) ClientM a
  } deriving (Functor, Applicative, Monad, MonadReader (Env TgBot), MonadIO)

type Token = Text

instance Bot TgBot where
    data Env TgBot = Env
        {envToken :: Token
        }

    type Update TgBot = Tg.Update
    type UpdateId TgBot = Tg.UpdateId

    getUpdates mUid = liftClient $ Tg.responseResult <$> Tg.getUpdates body
      where
        body :: Tg.GetUpdatesBody
        body = Tg.GetUpdatesBody mUid (Just Tg.UpdateMessage) (Just 25)

    getUpdateId ups
        | null ups = Nothing
        | otherwise = (1+) <$> updateId <?> last ups

    mkEnv = Env <$> getToken

    handleUpdate up =
        case updateToAction up of
            Just (Start cId) -> void . liftClient
                $ Tg.sendMessage (Tg.SendMessageBody cId startMessage)
            Just (EchoText cId t) -> void . liftClient
                $ Tg.sendMessage (Tg.SendMessageBody cId t)
            Just (EchoSticker cId s) -> void . liftClient
                $ Tg.sendSticker (Tg.SendStickerBody cId s)
            Nothing -> return ()

    unwrap env bot = do
        clientEnv <- defaultClientEnv env
        void $ runClientM (runReaderT (runTgBot bot) env) clientEnv
      where
        defaultClientEnv botEnv = mkClientEnv
            <$> newManager tlsManagerSettings
            <*> pure (botBaseUrl $ envToken botEnv)

data Action
    = Start Tg.ChatId
    | EchoText Tg.ChatId Text
    | EchoSticker Tg.ChatId Tg.FileId

updateToAction :: Update TgBot -> Maybe Action
updateToAction = runUpdateParser $ asum
    [ Start <$ command "start" <*> updateChatId
    , EchoText <$> updateChatId <*> text
    , EchoSticker <$> updateChatId <*> sticker
    ]

liftClient :: ClientM a -> TgBot a
liftClient = TgBot . lift

getToken :: IO Token
getToken = do
    conf <- load [Required "echobot.conf.local"]
    require conf "telegram.token"

botBaseUrl :: Token -> BaseUrl
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
