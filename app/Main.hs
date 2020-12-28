module Main where

import           Colog
import           System.Environment
import qualified Telegram.Bot       as Tg
import qualified Vk.Bot             as Vk

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
        "1" -> Tg.run richMessageAction
        "2" -> Vk.run richMessageAction
        "q" -> pure ()
        _   -> main

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
