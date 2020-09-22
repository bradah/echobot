{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot where

import           Control.Monad.Reader
import           Data.Foldable            (asum)
import qualified Data.Text                as T (Text, unlines)
import qualified Data.Text.IO             as TIO
import           Telegram.Env
import           Telegram.Methods
import           Telegram.Methods.Request
import           Telegram.Types
import           Telegram.UpdateParser

runBot :: Env -> IO ()
runBot e = startBotPolling Nothing e
  where
    startBotPolling :: Maybe UpdateId -> Env -> IO ()
    startBotPolling mUid env = do
      ups <- responseResult
        <$> runReaderT
          (getUpdates (GetUpdatesBody mUid (Just UpdateMessage) (Just 25)))
          env
      forM_ ups (handleUpdate env)
      let offset | null ups = Nothing
                 | otherwise = (1+) <$> updateId <?> last ups
      startBotPolling offset env

data Action
  = Start ChatId
  | EchoText ChatId T.Text
  | EchoSticker ChatId FileId

updateToAction :: Update -> Maybe Action
updateToAction = runUpdateParser $ asum
  [ Start <$ command "start" <*> updateChatId
  , EchoText <$> updateChatId <*> text
  , EchoSticker <$> updateChatId <*> sticker
  ]

handleUpdate :: Env -> Update -> IO ()
handleUpdate env up = do
  case updateToAction up of
    Just (Start cid) ->
      void $ runReaderT (sendMessage (SendMessageBody cid startMessage)) env
    Just (EchoText cid t) ->
      void $ runReaderT (sendMessage (SendMessageBody cid t)) env
    Just (EchoSticker cid s) ->
      void $ runReaderT (sendSticker (SendStickerBody cid s)) env
    Nothing -> return ()

startMessage :: T.Text
startMessage = T.unlines
  [ "Hi! This bot merely echoes your messages c:"
  , ""
  , "Supported messages:"
  , "- plain text"
  , "- stickers"
  , ""
  , "Supported commands:"
  , "- /start"
  ]
