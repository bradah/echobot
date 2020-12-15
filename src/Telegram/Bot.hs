{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}


module Telegram.Bot where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Configurator        as Conf
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text, unpack)
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Servant.Client

import           Telegram.Bot.Action
import           Telegram.Env
import qualified Telegram.Methods         as Methods
import           Telegram.Methods.Request
import           Telegram.ServMethods
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
          (Methods.getUpdates (GetUpdatesBody mUid (Just UpdateMessage) (Just 25)))
          env
      forM_ ups (handleUpdate env)
      let offset | null ups = Nothing
                 | otherwise = (1+) <$> updateId <?> last ups
      startBotPolling offset env

newtype BotM a = BotM
  { unBotM :: ReaderT BotSettings ClientM a
  } deriving (Functor, Applicative, Monad, MonadReader BotSettings, MonadIO)

runBotM :: BotSettings -> IO ()
runBotM sets = do
  clientEnv <- defaultClientEnv sets
  void $ unwrap mainLoop clientEnv

  where
    unwrap :: BotM () -> ClientEnv -> IO (Either ClientError ())
    unwrap bot = runClientM (runReaderT (unBotM bot) sets)

mainLoop :: BotM ()
mainLoop = startBotPolling Nothing
  where
    startBotPolling :: Maybe UpdateId -> BotM ()
    startBotPolling mUid = do
      BotSettings{..} <- ask
      let upBody = GetUpdatesBody
            mUid
            (Just UpdateMessage)
            (Just 25)
          processUpdate up = fromMaybe
            (pure ())
            (botHandleUpdate up >>= botHandleAction)
      ups <- responseResult <$> liftClient (getUpdates upBody)
      forM_ ups processUpdate
      let newMUid | null ups = Nothing
                  | otherwise = (1+) <$> updateId <?> last ups
      startBotPolling newMUid

liftClient :: ClientM a -> BotM a
liftClient = BotM . lift

data BotSettings = BotSettings
  { botHandleUpdate :: Update -> Maybe Action
  , botHandleAction :: Action -> Maybe (BotM ())
  , botToken        :: Token
  }

mkBotSettings
  :: (Update -> Maybe Action)
  -> (Action -> Maybe (BotM ()))
  -> IO BotSettings
mkBotSettings upHandler actHandler =
  BotSettings upHandler actHandler <$> getToken

defaultClientEnv :: BotSettings -> IO ClientEnv
defaultClientEnv BotSettings{..} = mkClientEnv
  <$> newManager tlsManagerSettings
  <*> pure (botBaseUrl botToken)


getToken :: IO Token
getToken = do
  conf <- Conf.load [Conf.Required "echobot.conf.local"]
  Conf.require conf "telegram.token"

{- runBot :: BotSettings -> IO ()
runBot sets = do
  let token = botToken sets -}

botBaseUrl :: Token -> BaseUrl
botBaseUrl token = BaseUrl
  Https
  "api.telegram.org"
  443
  (unpack $ "/bot" <> token)
