{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Reader
import           Data.Foldable            (asum)
import qualified Data.Text                as T (Text, unlines)
import qualified Data.Text.IO             as TIO
import           Telegram.Env
import           Telegram.Methods
import           Telegram.Methods.Request
import           Telegram.Types
import           Telegram.UpdateParser

main :: IO ()
main = do
  putStrLn "Please enter your bot token"
  token <- TIO.getLine
  let env = mkEnv token
  go Nothing env

    where
        go :: Maybe UpdateId -> Env -> IO ()
        go mUid env = do
            ups <- responseResult
                <$> runReaderT
                    (getUpdates (GetUpdatesBody mUid (Just UpdateMessage) (Just 60)))
                    env
            forM_ ups (handleUpdate env)
            let offset | null ups = Nothing
                       | otherwise = (1+) <$> updateId <?> last ups
            go offset env

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
