{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Vk.Methods where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashMap.Lazy    as Map
import           Data.Text            (Text)

import           API.Logging
import           API.Utils
import           Vk.Internal.Bot
import qualified Vk.Internal.Methods  as Int
import           Vk.Internal.Request
import           Vk.Internal.Types
import           Vk.UpdateParser

getUpdates :: Bot [Update]
getUpdates = do
    logInfo "Waiting for updates..."
    params <- mkParams
    resp <- liftClient $ Int.checkLps params
    let ups = checkLpsResponseUpdates resp
    logInfo $ "Updates received: " <> showP (length ups)
    logDebug $ "Response: " <> showP resp
    putNewTs resp
    conversations <- addConversations ups
    modify (\st -> st {bStateConversations = conversations})
    logDebug $ "Current conversations: " <> showP conversations
    pure ups

  where
    mkParams :: Bot CheckLpsParams
    mkParams = do
        ts <- gets bStateTs
        Env{..} <- ask
        pure $ CheckLpsParams envLpsServer ACheck envLpsKey (Just 25) ts

    putNewTs :: CheckLpsResponse -> Bot ()
    putNewTs resp = case checkLpsResponseTs resp of
        Just ts -> modify (\st -> st {bStateTs = ts})
        Nothing -> logWarning "No new Ts. Proceeding with an old one."

    addConversations :: [Update] -> Bot ConvMap
    addConversations ups = do
        convMap <- gets bStateConversations
        let uIds = extractUserId ups
        return $ insertIfNew uIds convMap
      where
        insertIfNew :: [UserId] -> ConvMap -> ConvMap
        insertIfNew [] convs = convs
        insertIfNew (u:us) convs =
            if not $ u `Map.member` convs
                then
                    Map.insert
                        u
                        (Conversation defaultRepeatNumber)
                        convs
                else
                    insertIfNew us convs

        extractUserId :: [Update] -> [UserId]
        extractUserId [] = []
        extractUserId (u:us) = case updateUserId <?> u of
            Just cid -> cid : extractUserId us
            Nothing  -> extractUserId us


sendMessage :: UserId -> Text -> [Attachment] -> Bot ()
sendMessage userId t atts = do
    botEnv <- ask
    repeatNum <- gets ((convRepeatNum . (Map.! userId)) . bStateConversations)
    logInfo $ "Sending message: " <!> showP (params botEnv)
    resp <- head <$> replicateM repeatNum
        (liftClient $ Int.sendMessage $ params botEnv)
    logDebug $ "Response: " <> showT resp
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
    repeatNum <- gets ((convRepeatNum . (Map.! userId)) . bStateConversations)
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
