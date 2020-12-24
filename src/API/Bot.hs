{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module API.Bot
    ( module API.Bot.Class
    , runBot
    ) where

import           Colog

import           API.Bot.Class
import           Data.Foldable (forM_)
import           Data.Text     (pack)


runBot :: forall b . Bot b => IO ()
runBot = do
    env <- mkEnv
    unwrap env $ logInfo $ "Loaded environment: " <> pack (show env)
    unwrap env $ startBotPolling Nothing
  where
    startBotPolling :: Maybe (UpdateId b) -> b a
    startBotPolling mUid = do
        ups <- getUpdates mUid
        forM_ ups handleUpdate
        nextUid <- getUpdateId ups
        startBotPolling nextUid
