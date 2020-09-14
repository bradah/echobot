{-# LANGUAGE OverloadedStrings #-}
module Telegram.Methods where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Char8  as B
import           Network.HTTP.Req
import           Telegram.Request
import           Telegram.Types

-- * Availabe methods

-- ** getMe

-- | A simple method for testing your bot's auth token.
--   Requires no parameters. Returns basic information about
--   the bot in form of a 'User' object.

type Method body res = ReaderT (Payload body) IO (Response res)

data Payload body = Payload
  {
    payloadToken       :: Token
  , payloadRequestBody :: Maybe body
  }

runMethod :: Method body res -> Payload body -> IO (Response res)
runMethod = runReaderT

getMe :: Method a User
getMe = do
    token <- asks payloadToken
    runReq defaultHttpConfig $ do
        resp <- req GET
                (botBaseUrl token /: "getMe")
                NoReqBody
                jsonResponse
                mempty
        liftIO . return $ (responseBody resp)

-- ** sendMessage

-- | Use this method to send text messages.
--   On success, the sent 'Message' is returned.
sendMessage :: Method SendMessageRequest Message
sendMessage = do
    token <- asks payloadToken
    reqbody <- asks payloadRequestBody
    runReq defaultHttpConfig $ do
        json <- req POST
                (botBaseUrl token /: "sendMessage")
                (ReqBodyJson reqbody)
                jsonResponse
                mempty
        liftIO . return $ (responseBody json)

-- ** getUpdates

-- | Use this method to receive incoming updates.
--   An Array of 'Update' objects is returned.
getUpdates :: Method GetUpdatesRequest [Update]
getUpdates = do
    token <- asks payloadToken
    reqbody <- asks payloadRequestBody
    runReq defaultHttpConfig $ do
        resp <- req POST
                (botBaseUrl token /: "getUpdates")
                (ReqBodyJson reqbody)
                jsonResponse
                mempty
        liftIO . return $ (responseBody resp)

-- ** sendSticker

-- | Use this method to send static .WEBP or
--   animated .TGS stickers. On success, the sent
--   'Message' is returned.

sendSticker :: Method SendMessageRequest [Message]
sendSticker = do
    token <- asks payloadToken
    reqbody <- asks payloadRequestBody
    runReq defaultHttpConfig $ do
        resp <- req POST
                    (botBaseUrl token /: "sendSticker")
                    (ReqBodyJson reqbody)
                    jsonResponse
                    mempty
        liftIO . return $ (responseBody resp)
