module Main where

import           Colog
import           Control.Monad.IO.Class (MonadIO)
import           System.Environment
import           System.IO
import qualified Telegram.Bot           as Tg
import qualified Vk.Bot                 as Vk

main :: IO ()
main = do
    handle <- openFile "echobot.log" AppendMode
    hSetBuffering handle NoBuffering
    putStr $ unlines
        [ "To start, specify a number of bot you want to run"
        , "1: Telegram bot"
        , "2: Vk bot"
        , "q: Quit"
        ]
    arg <- getLine
    case arg of
        "1" -> Tg.run $ logAction Debug handle
        "2" -> Vk.run $ logAction Debug handle
        "q" -> pure ()
        _   -> main
    hClose handle

logAction :: MonadIO m => Severity -> Handle -> LogAction m Message
logAction sev handle = cfilter ((>= sev) . msgSeverity) (richMessageAction <> richMessageToFileAction)
  where
    richMessageToFileAction :: MonadIO m => LogAction m Message
    richMessageToFileAction = upgradeMessageAction defaultFieldMap $
        cmapM fmtRichMessageDefault (logTextHandle handle)

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
