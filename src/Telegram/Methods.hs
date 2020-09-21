{-# LANGUAGE OverloadedStrings #-}
module Telegram.Methods where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Network.HTTP.Req
import           Telegram.Env
import           Telegram.Methods.Request
import           Telegram.Types

-- * Available 'Method's

-- ** getMe

-- | A simple method for testing your bot's
-- auth token. Requires no parameters. Returns
-- basic information about the bot in form of
-- a 'User' object.

getMe :: ReaderT Env IO (Response User)
getMe = do
    baseUrl <- asks envBaseUrl
    runReq defaultHttpConfig $ do
        resp <- req GET
                    (baseUrl /: "getMe")
                    NoReqBody
                    jsonResponse
                    mempty
        liftIO . return $ (responseBody resp)

-- ** sendMessage

-- | Use this method to send text messages.
-- On success, the sent 'Message' is returned.

sendMessage :: SendMessageBody -> ReaderT Env IO (Response Message)
sendMessage body = do
    baseUrl <- asks envBaseUrl
    runReq defaultHttpConfig $ do
        resp <- req POST
                    (baseUrl /: "sendMessage")
                    (ReqBodyJson body)
                    jsonResponse
                    mempty
        liftIO . return $ (responseBody resp)

-- ** sendSticker

-- | Use this method to send static .WEBP or animated
-- .TGS stickers. On success, the sent 'Message' is returned.

sendSticker :: SendStickerBody -> ReaderT Env IO (Response Message)
sendSticker body = do
    baseUrl <- asks envBaseUrl
    runReq defaultHttpConfig $ do
        resp <- req POST
                    (baseUrl /: "sendSticker")
                    (ReqBodyJson body)
                    jsonResponse
                    mempty
        liftIO . return $ (responseBody resp)

-- ** getUpdates

-- | Use this method to receive incoming updates.
-- An Array of 'Update' objects is returned.

getUpdates :: GetUpdatesBody -> ReaderT Env IO (Response [Update])
getUpdates body = do
    baseUrl <- asks envBaseUrl
    runReq defaultHttpConfig $ do
        resp <- req POST
                    (baseUrl /: "getUpdates")
                    (ReqBodyJson body)
                    jsonResponse
                    mempty
        liftIO . return $ (responseBody resp)
