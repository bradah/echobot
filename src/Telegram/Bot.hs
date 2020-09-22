{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot where

import           Control.Monad.Reader
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
runBot e = startBotPolling Nothing e
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
