{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module VK.Methods where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Network.HTTP.Req
import           System.Random          (randomIO)
import           VK.Env
import           VK.Methods.Request
import           VK.Types


checkLPS :: Env -> IO Response
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
      mempty
