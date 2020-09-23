{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Configurator as Conf
import qualified Data.Text.IO      as TIO
import           Telegram.Bot
import           Telegram.Env

main :: IO ()
main = do
  -- putStrLn "Please enter your bot token"
  -- token <- TIO.getLine
  conf <- Conf.load [Conf.Required "echobot.conf.local"]
  mbToken <- Conf.lookup conf "telegram.token"
  maybe (return ()) (runBot . mkEnv) mbToken
