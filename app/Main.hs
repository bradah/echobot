module Main where

import           API.Logging
import           System.IO
import           Telegram.Bot

main :: IO ()
main = do
    handle <- openFile "echobot.log" AppendMode
    run $ logStdOutAndFile Info handle
    hClose handle
