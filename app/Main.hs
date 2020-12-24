{-# LANGUAGE TypeApplications #-}

module Main where

import           API.Bot
import           Telegram.Bot
import           Vk.Bot

main :: IO ()
main = do
    putStr $ unlines
        [ "To start, specify a number of bot you want to run"
        , "1: Telegram bot"
        , "2: Vk bot"
        , "q: Quit"
        ]
    arg <- getLine
    case arg of
        "1" -> runBot @TgBot
        "2" -> runBot @VkBot
        "q" -> pure ()
        _   -> main
