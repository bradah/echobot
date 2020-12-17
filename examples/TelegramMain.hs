{-# LANGUAGE TypeApplications #-}
module Main where

import           API.Bot
import           Telegram.Bot

main :: IO ()
main = runBot @TgBot
