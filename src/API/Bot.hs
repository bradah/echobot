{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module API.Bot where

import           API.Bot.Class
import           Data.Foldable (forM_)

runBot :: forall b . Bot b => IO ()
runBot = do
    env <- mkEnv
    unwrap env $ startBotPolling Nothing
  where
    startBotPolling :: Maybe (UpdateId b) -> b a
    startBotPolling mUid = do
        ups <- getUpdates mUid
        forM_ ups handleUpdate
        nextUid <- getUpdateId ups
        startBotPolling nextUid
