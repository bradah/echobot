{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module VK.Methods where

import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import           Network.HTTP.Req
import           VK.Env


getUpdates :: Env -> IO ByteString
getUpdates Env{..} = runReq defaultHttpConfig $ do
  let params = "act" =: ("a_check" :: Text)
                <>"key" =: envLPSKey
                <> "ts" =: envTs
                <> "wait" =: (0 :: Int)
  resp <- req POST
              envLPSServer
              (ReqBodyUrlEnc params)
              bsResponse
              mempty
  liftIO . return $ responseBody resp
