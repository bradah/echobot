module Main where

import           API.Logging
import           System.IO
import           Telegram.Bot

main :: IO ()
main = do
    handle <- openFile "echobot.log" AppendMode
    hSetBuffering handle NoBuffering
    run $ logStdOutAndFile Debug handle
    hClose handle

-- | Release version

{- main :: IO ()
main = do
    args <- getArgs
    case head args of
        "tg" -> Tg.run richMessageAction
        "vk" -> Vk.run richMessageAction
        "q"  -> pure ()
        _    -> main
 -}
