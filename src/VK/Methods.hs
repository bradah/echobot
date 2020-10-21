{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module VK.Methods where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Network.HTTP.Req
import           VK.Env
import           VK.Methods.Request


getUpdates :: Env -> IO GetUpdatesResponse
getUpdates Env{..} = runReq defaultHttpConfig $ do
  let params = "act" =: ("a_check" :: Text)
                <> "key" =: envLPSKey
                <> "ts" =: envTs
                <> "wait" =: (25 :: Int)
  resp <- req POST
              (https "lp.vk.com" /: envLPSServer)
              (ReqBodyUrlEnc params)
              jsonResponse
              mempty
  liftIO . return $ responseBody resp
