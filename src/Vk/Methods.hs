module Vk.Methods where

import           AppState
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Data.Maybe
import           Data.Text                 (Text)
import           Eff.Https                 hiding (get)
import           Eff.Log                   hiding (Config)
import           Eff.Random
import           Vk.Config
import           Vk.Data
import           Vk.Parser
import           Vk.Requests


type Method r = Members
    [ Https
    , Log
    , State Config
    , State VkState
    , Random
    ] r

type MethodWithCallStack r = (Method r, HasCallStack)

getLps :: MethodWithCallStack r => Eff r GetLpsResult
getLps = do
    params <- mkParams
    logInfo "Requesting Long Poll Server"
    resp <- post (https "api.vk.com"
                     /: "method"
                     /: "groups.getLongPollServer"
                     )
                 params
    logDebug $ "Response: " <+> resp
    pure resp
  where
    mkParams
        :: Members [State Config, State VkState] r
        => Eff r ReqBodyUrlEnc
    mkParams = do
        Config {..} <- get
        pure . url $
               "group_id" =: groupId
            <> "access_token" =: token
            <> "v" =: (5.122 :: Float)


checkLps :: MethodWithCallStack r => Eff r CheckLpsResponse
checkLps = do
    logInfo "Waiting for updates..."
    params <- mkParams
    server <- gets (fromJust . checkLpsServer)
    resp <- post server params
    let ups = checkLpsResp'updates resp
    logInfo $ "Updates received: " <+> length ups
    logDebug $ "Response: " <+> resp
    putNewTs resp
    sessions <- putNewSessions ups
    logDebug $ "Current sessions: " <+> sessions
    pure resp

  where
    mkParams :: Members [State Config, State VkState] r => Eff r ReqBodyUrlEnc
    mkParams = do
        ts <- (gets @VkState) st'offset
        Config {..} <- get
        pure . url $
               "act" =: ACheck
            <> "key" =: checkLpsKey
            <> "wait" =: (25 :: Int)
            <> "ts" =: ts

    putNewTs :: Members [Log, State VkState] r => CheckLpsResponse -> Eff r ()
    putNewTs resp = case checkLpsResp'ts resp of
        Just ts -> modify (\st -> st { st'offset = ts })
        Nothing -> logWarning "No new Ts. Proceeding with an old one."

    putNewSessions :: Member (State VkState) r => [Update] -> Eff r SesMap
    putNewSessions [] = (gets @VkState) st'sessions
    putNewSessions (u:us) = case u <?> updateUserId of
            Just uid -> (modify @VkState) (newSession uid) >> putNewSessions us
            _        -> putNewSessions us
sendMessage
    :: Method r
    => Int -- ^ User id
    -> Text
    -> [Attachment]
    -> Eff r SendMessageResponse
sendMessage userId t atts = do
    repNum <- (gets @VkState) $ getRepNum userId
    params <- mkParams
    logInfo $ "Sending message to user"
        <+> userId <> " (repeat: " <+> repNum <> ")"
    resp <- fmap head . replicateM repNum $
        post (https "api.vk.com"
                 /: "method"
                 /: "messages.send"
             ) params
    logDebug $ "Response: " <+> resp
    pure resp
  where
    mkParams = do
        Config {..} <- get
        rand <- (`mod` 1000000) <$> random
        logDebug $ "RanomdId = " <+> rand
        pure . url $
               "user_id" =: userId
            <> "random_id" =: rand
            <> "message" =: t
            <> "attachment" =: atts
            <> "access_token" =: token
            <> "v" =: (5.122 :: Float)

sendSticker :: Method r
            => Int -- ^ User id
            -> Int -- ^ Sticker id
            -> Eff r SendMessageResponse
sendSticker userId s = do
    repNum <- (gets @VkState) $ getRepNum userId
    params <- mkParams
    logInfo $ "Sending sticker to user"
        <+> userId <> " (repeat: " <+> repNum <> ")"
    resp <- fmap head . replicateM repNum $
        post (https "api.vk.com"
                 /: "method"
                 /: "messages.send"
             ) params
    logDebug $ "Response: " <+> resp
    pure resp
  where
    mkParams = do
        Config {..} <- get
        rand <- (`mod` 1000000) <$> random
        logDebug $ "RanomdId = " <+> rand
        pure . url $
               "user_id" =: userId
            <> "random_id" =: rand
            <> "sticker_id" =: s
            <> "access_token" =: token
            <> "v" =: (5.122 :: Float)



