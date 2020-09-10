{-# LANGUAGE OverloadedStrings #-}
module Telegram.Methods where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8  as B
import           Network.HTTP.Req
import           Telegram.Request
import           Telegram.Types

-- * This file contains availabe methods

-- ** getMe

-- | A simple method for testing your bot's auth token.
--   Requires no parameters. Returns basic information about
--   the bot in form of a 'User' object.
getMe :: Token -> IO (Response User)
getMe token = runReq defaultHttpConfig $ do
  resp <- req GET
            (botBaseUrl token /: "getMe")
            NoReqBody
            jsonResponse
            mempty
  liftIO . return $ (responseBody resp)

-- ** sendMessage

-- | Use this method to send text messages.
--   On success, the sent 'Message' is returned.
sendMessage :: Token -> SendMessageRequest -> IO (Response Message)
sendMessage token reqbody = runReq defaultHttpConfig $ do
  json <- req POST
            (botBaseUrl token /: "sendMessage")
            (ReqBodyJson reqbody)
            jsonResponse
            mempty
  liftIO . return $ (responseBody json)

-- ** getUpdates

-- | Use this method to receive incoming updates.
--   An Array of 'Update' objects is returned.
getUpdates :: Token -> GetUpdatesRequest -> IO (Response [Update])
getUpdates token reqbody = runReq defaultHttpConfig $ do
  resp <- req POST
            (botBaseUrl token /: "getUpdates")
            (ReqBodyJson reqbody)
            jsonResponse
            mempty
  liftIO . return $ (responseBody resp)
