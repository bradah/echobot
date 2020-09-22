{-# LANGUAGE OverloadedStrings #-}
module Telegram.UpdateParserSpec where

import qualified Data.Text                as T (isPrefixOf, tail, unwords,
                                                words)
import           Telegram.Types
import           Telegram.Types.Arbitrary ()
import           Telegram.UpdateParser
import           Test.Hspec
import           Test.Hspec.QuickCheck


textSpec :: Spec
textSpec =
  prop "extracts text from Update" $
    \arbU -> case updateMessage arbU >>= messageText of
      Nothing -> (text <?> arbU) == Nothing
      Just t  -> (text <?> arbU) == Just t

plainTextSpec :: Spec
plainTextSpec =
  context "when given an Update without command" $
    prop "extracts text from Update" $
      \arbU -> case updateMessage arbU >>= messageText of
        Nothing -> (plainText <?> arbU) == Nothing
        Just t -> if "/" `T.isPrefixOf` t
          then (plainText <?> arbU) == Nothing
          else (plainText <?> arbU) == Just t

commandSpec :: Spec
commandSpec =
  context "when given an Update with command" $
    prop "extracts command's arguments" $
      \arbU arbT -> case updateMessage arbU >>= messageText of
        Nothing -> (command arbT <?> arbU) == Nothing
        Just t -> if "/" `T.isPrefixOf` t
          then
            case T.words t of
            (w:ws) ->
              (command (T.tail w) <?> arbU) == Just (T.unwords ws)
            _      ->
              (command arbT <?> arbU) == Nothing
          else
            (command arbT <?> arbU) == Nothing

stickerSpec :: Spec
stickerSpec =
  context "when given an Update with Sticker" $
    prop "extracts FileId of Sticker" $
      \arbU -> case updateMessage arbU >>= messageSticker of
        Nothing -> (sticker <?> arbU) == Nothing
        Just s  -> (sticker <?> arbU) == Just (stickerFileId s)

updateChatIdSpec :: Spec
updateChatIdSpec =
  context "when given an Update with Message" $
    prop "extracts Message's ChatId" $
      \arbU -> case updateMessage arbU of
        Nothing -> (updateChatId <?> arbU) == Nothing
        Just m  ->
          (updateChatId <?> arbU) == Just (chatId . messageChat $ m)

updateIdSpec :: Spec
updateIdSpec =
  context "when given an Update" $
    prop "extracts its UpdateId" $
      \arbU -> (updateId <?> arbU) == Just (updateUpdateId arbU)

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  describe "text" textSpec
  describe "plainText" plainTextSpec
  describe "command" commandSpec
  describe "sticker" stickerSpec
  describe "updateChatId" updateChatIdSpec
  describe "updateId" updateIdSpec
