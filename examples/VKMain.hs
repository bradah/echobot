{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Configurator as Conf
import           VK.Bot
import           VK.Env

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "echobot.conf.local"]
  mbToken <- Conf.lookup conf "vk.token"
  mbGroupId <- Conf.lookup conf "vk.group_id"
  case mbToken of
    Nothing -> error "echobot.conf.local: no vk.token"
    Just token -> case mbGroupId of
      Nothing  -> error "echobot.conf.local: no vk.group_id"
      Just gId -> do
        env <- mkEnv token gId
        print env
