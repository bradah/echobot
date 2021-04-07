{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Vk.Methods where

{- import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Text            (Text)

import           Log              hiding (Message)
import           Bot.State
import           Bot.Utils
import           Vk.Internal.Bot
import           Vk.Internal.Data
import qualified Vk.Internal.Methods  as Int
import           Vk.Internal.Request
import           Vk.Parser

getUpdates :: Bot [Update]
getUpdates = do
    logInfo "Waiting for updates..."
    params <- mkParams
    resp <- liftClient $ Int.checkLps params
    let ups = checkLpsResponseUpdates resp
    logInfo $ "Updates received: " <> showP (length ups)
    logDebug $ "Response: " <> showP resp
    putNewTs resp
    sessions <- modifySessions ups
    logDebug $ "Current sessions: " <> showP sessions
    pure ups

  where
    mkParams :: Bot CheckLpsParams
    mkParams = do
        ts <- gets offset
        Env{..} <- ask
        pure $ CheckLpsParams envLpsServer ACheck envLpsKey (Just 25) ts

    putNewTs :: CheckLpsResponse -> Bot ()
    putNewTs resp = case checkLpsResponseTs resp of
        Just ts -> modify (\st -> st { offset = ts })
        Nothing -> logWarning "No new Ts. Proceeding with an old one."

    modifySessions :: [Update] -> Bot (SessionMap Message)
    modifySessions [] = gets getSesMap
    modifySessions (u:us) = case u <?> updateUserId of
            Just uid -> modify (newSession uid) >> modifySessions us
            _        -> modifySessions us

sendMessage :: UserId -> Text -> [Attachment] -> Bot ()
sendMessage userId t atts = do
    botEnv <- ask
    repeatNum <- gets (repNum . (! userId))
    logInfo $ "Sending message: " <!> showP (params botEnv)
    resp <- head <$> replicateM repeatNum
        (liftClient $ Int.sendMessage $ params botEnv)
    logDebug $ "Response: " <!> showT resp
  where
    params :: Env -> SendMessageParams
    params Env{..} = SendMessageParams
        userId
        (Just t)
        Nothing
        atts
        envToken
        envApiVersion

sendSticker :: UserId -> StickerId -> Bot ()
sendSticker userId s = do
    botEnv <- ask
    repeatNum <- gets (repNum . (! userId))
    logInfo $ "Sending sticker: " <!> showP (params botEnv)
    resp <- head <$> replicateM repeatNum
        (liftClient $ Int.sendMessage $ params botEnv)
    logDebug $ "Response: " <> showT resp
  where
    params :: Env -> SendMessageParams
    params Env{..} = SendMessageParams
        userId
        Nothing
        (Just s)
        []
        envToken
        envApiVersion
 -}