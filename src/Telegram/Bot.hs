{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}


module Telegram.Bot where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Configurator        as Conf
import           Data.Text                (Text, unpack)
import           Servant.Client

import           Telegram.Bot.Action
import           Telegram.Env
import           Telegram.Methods
import           Telegram.Methods.Request
import           Telegram.Types
import           Telegram.UpdateParser

-- * Bot logic

-- ** runBot

-- | Start bot with long polling.

runBot :: Env -> IO ()
runBot = startBotPolling Nothing
  where
    startBotPolling :: Maybe UpdateId -> Env -> IO ()
    startBotPolling mUid env = do
      ups <- responseResult
        <$> runReaderT
          (getUpdates (GetUpdatesBody mUid (Just UpdateMessage) (Just 25)))
          env
      forM_ ups (handleUpdate env)
      let offset | null ups = Nothing
                 | otherwise = (1+) <$> updateId <?> last ups
      startBotPolling offset env


newtype BotM a = BotM
  { unBotM :: ReaderT BotSettings ClientM a
  } deriving (Functor, Applicative, Monad, MonadReader BotSettings, MonadIO)



data BotSettings = BotSettings
  { botHandleUpdate :: Update -> Maybe Action
  , botHandleAction :: Action -> BotM ()
  , botToken        :: Text
  }

mkBotSettings
  :: (Update -> Maybe Action)
  -> (Action -> BotM ())
  -> IO BotSettings
mkBotSettings upHandler actHandler = do
  conf <- Conf.load [Conf.Required "echobot.conf.local"]
  BotSettings upHandler actHandler
    <$> Conf.require conf "telegram.token"


{- runBot :: BotSettings -> IO ()
runBot sets = do
  let token = botToken sets -}

botBaseUrl :: Token -> BaseUrl
botBaseUrl token = BaseUrl Https "api.telegram.org" 443
  (unpack ("/bot" <> token))
