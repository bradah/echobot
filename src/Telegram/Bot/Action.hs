{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.Action where

import           Control.Monad.Reader
import           Data.Foldable            (asum)
import qualified Data.Text                as T (Text, unlines)
import           Telegram.Env
import           Telegram.Methods
import           Telegram.Methods.Request
import           Telegram.Types
import           Telegram.UpdateParser

-- * Bot actions

-- ** Action

-- | Available bot actions.

data Action
  = Start ChatId
  -- ^ Send greeting message with instructions.
  | EchoText ChatId T.Text
  -- ^ Echo plain text.
  | EchoSticker ChatId FileId
  -- ^ Echo 'Sticker'.

-- ** updateToAction

-- | Map proper 'Action' to given 'Update'.

updateToAction :: Update -> Maybe Action
updateToAction = runUpdateParser $ asum
  [ Start <$ command "start" <*> updateChatId
  , EchoText <$> updateChatId <*> text
  , EchoSticker <$> updateChatId <*> sticker
  ]

-- ** handleUpdate

-- | Map proper method to given 'Update'.

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
