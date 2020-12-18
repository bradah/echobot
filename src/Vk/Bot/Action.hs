{-# LANGUAGE OverloadedStrings #-}
module Vk.Bot.Action where

import           Control.Monad.Reader
import           Data.Foldable        (asum)
import qualified Data.Text            as T (Text, unlines)
import           Vk.Env
import           Vk.Methods
import           Vk.Methods.Request
import           Vk.Types
import           Vk.UpdateParser

-- * Bot actions

-- ** Action

-- | Available bot actions.

data Action
  = Start UserId
  -- ^ Send greeting message with instructions.
  | EchoText UserId T.Text
  -- ^ Echo plain text.
--   | EchoSticker UserId FileId
  -- ^ Echo 'Sticker'.

-- ** updateToAction

-- | Map proper 'Action' to given 'Update'.

updateToAction :: Update -> Maybe Action
updateToAction = runUpdateParser $ asum
  [ Start <$ command "start" <*> updateUserId
  , EchoText <$> updateUserId <*> text
--   , EchoSticker <$> updateChatId <*> sticker
  ]

-- ** handleUpdate

-- | Map proper method to given 'Update'.

{- handleUpdate :: Env -> Update -> IO ()
handleUpdate env up = do
  case updateToAction up of
    Just (Start uid) ->
      void $ sendMessage env uid startMessage
    Just (EchoText uid t) ->
      void $ sendMessage env uid t
    -- Just (EchoSticker cid s) ->
    --   void $ runReaderT (sendSticker (SendStickerBody cid s)) env
    Nothing -> return ()
 -}
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

