module Main where

import           API.Logging
import           System.IO
import qualified Telegram.Bot as Tg
import qualified Vk.Bot       as Vk

main :: IO ()
main = do
    handle <- openFile "echobot.log" WriteMode
    hSetBuffering handle NoBuffering
    putStr $ unlines
        [ "To start, specify a number of bot you want to run"
        , "1: Telegram bot"
        , "2: Vk bot"
        , "q: Quit"
        ]
    arg <- getLine
    case arg of
        "1" -> Tg.run $ logStdOutAndFile Debug handle
        "2" -> Vk.run $ logStdOutAndFile Debug handle
        "q" -> pure ()
        _   -> main
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
