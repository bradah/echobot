module Main where

import           API.Logging
import           Control.Exception (bracket)
import           Control.Monad     (forever)
import           System.Exit       (exitSuccess)
import           System.IO
import qualified Telegram.Bot      as Tg
import qualified Vk.Bot            as Vk

main :: IO ()
main = forever $ bracket acquire release loop
  where
    acquire :: IO Handle
    acquire = do
        handle <- openFile "echobot.log" WriteMode
        hSetBuffering handle NoBuffering
        return handle

    release :: Handle -> IO ()
    release = hClose

    loop :: Handle -> IO ()
    loop handle = do
        putStr $ unlines
            [ "To start, specify a number of bot you want to run"
            , "1: Telegram bot"
            , "2: Vk bot"
            , "q: Quit"
            ]
        arg <- getLine
        case arg of
            "1" -> Tg.run $ logStdOut Debug
            "2" -> Vk.run $ logStdOut Debug
            "q" -> exitSuccess
            _   -> pure ()
