{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Configurator        as Conf
import           Telegram.Bot
import           Telegram.Env


import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Servant.Client
import           Telegram.Bot
import           Telegram.Methods.Request
import           Telegram.ServMethods

main :: IO ()
main = do
  -- putStrLn "Please enter your bot token"
  -- token <- TIO.getLine
  conf <- Conf.load [Conf.Required "echobot.conf.local"]
  mbToken <- Conf.lookup conf "telegram.token"
  maybe (return ()) (runBot . mkEnv) mbToken

f :: IO ()
f = do
  conf <- Conf.load [Conf.Required "echobot.conf.local"]
  token <- Conf.require conf "telegram.token"
  let base = botBaseUrl token
  clientEnv <- mkClientEnv <$> newManager tlsManagerSettings <*> pure base
  res <- runClientM (getMe >> getUpdates (GetUpdatesBody Nothing Nothing Nothing)) clientEnv
  case res of
    Right x -> print x
    _       -> undefined
  return ()
