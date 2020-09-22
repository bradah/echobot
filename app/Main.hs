{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import           Telegram.Bot
import           Telegram.Env

main :: IO ()
main = do
  putStrLn "Please enter your bot token"
  token <- TIO.getLine
  let env = mkEnv token
  runBot env
