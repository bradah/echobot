{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Vk.Methods where

import           Control.Monad.IO.Class
import           Data.Text              (Text, unpack)
-- import           Network.HTTP.Req
import           System.Random          (randomIO)
import           Vk.Env
import           Vk.Types


import           Control.Monad.Reader   (local)
import           Data.Proxy
import           GHC.TypeLits
import           Servant.API
import           Servant.Client         hiding (Response)
import           Vk.Methods.Request

{- checkLPS :: Env -> IO Response
checkLPS Env{..} = runReq defaultHttpConfig $ do
  let params = "act" =: ("a_check" :: Text)
            <> "key" =: envLPSKey
            <> "wait" =: (25 :: Int)
            <> "ts" =: envTs
  resp <- req POST
              envLPSServer
              (ReqBodyUrlEnc params)
              jsonResponse
              mempty
  liftIO . return $ responseBody resp

getUpdates :: Env -> IO [Update]
getUpdates env = responseUpdates <$> checkLPS env

sendMessage :: Env -> UserId -> Text -> IO IgnoreResponse
sendMessage Env{..} uid text = runReq defaultHttpConfig $ do
  randId <- liftIO (randomIO :: IO Int)
  let params = "user_id" =: uid
            <> "random_id" =: randId
            <> "message" =: text
            <> "access_token" =: envToken
            <> "v" =: envApiVersion
  req POST
      (https "api.vk.com" /: "method" /: "messages.send")
      (ReqBodyUrlEnc params)
      ignoreResponse
      mempty -}

type RequiredParam = QueryParam' '[Required, Strict]
type OptionalParam = QueryParam' '[Optional, Strict]

type CheckLps
    =  RequiredParam "act" CheckLpsAction
    :> RequiredParam "key" LpsKey
    :> OptionalParam "wait" Int
    :> RequiredParam "ts" Ts
    :> Post '[JSON] CheckLpsResponse

checkLps :: CheckLpsParams -> ClientM CheckLpsResponse
checkLps CheckLpsParams{..} = do
    burl <- parseBaseUrl (unpack checkLpsServer)
    local (\e -> e { baseUrl = burl }) $
        client
            (Proxy @CheckLps)
            checkLpsAction
            checkLpsKey
            checkLpsWait
            checkLpsTs

getUpdates :: CheckLpsParams -> ClientM [Update]
getUpdates = fmap checkLpsResponseUpdates . checkLps

type VkMethod (verb :: [*] -> * -> *) (response :: *)
    =  RequiredParam "access_token" Token
    :> RequiredParam "v" Double
    :> verb '[JSON] response

type SendMessage
    =  "messages.send"
    :> RequiredParam "user_id" UserId
    :> RequiredParam "random_id" Int
    :> RequiredParam "message" Text
    :> VkMethod Post SendMessageResponse

sendMessage :: SendMessageParams -> ClientM SendMessageResponse
sendMessage SendMessageParams{..} = do
    randId <- liftIO randomIO
    client
        (Proxy @SendMessage)
        sendMessageUserId
        (randId :: Int)
        sendMessageMessage
        sendMessageAccessToken
        sendMessageApiVersion

type GetLps
    =  "groups.getLongPollServer"
    :> RequiredParam "group_id" Integer
    :> VkMethod Post GetLpsResult

getLps :: GetLpsParams -> ClientM GetLpsResult
getLps GetLpsParams{..} = client
    (Proxy @GetLps)
    getLpsGroupId
    getLpsToken
    getLpsApiVersion
