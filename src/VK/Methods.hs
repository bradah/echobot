{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module VK.Methods where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           Network.HTTP.Req
import           VK.Env
import           VK.Methods.Request


checkLPS :: Env -> IO Response
checkLPS Env{..} = runReq defaultHttpConfig $ do
  let params = "act" =: ("a_check" :: Text)
              <>"key" =: envLPSKey
              <> "wait" =: (25 :: Int)
              <> "ts" =: envTs
  resp <- req POST
              envLPSServer
              (ReqBodyUrlEnc params)
              jsonResponse
              mempty
  liftIO . return $ responseBody resp
