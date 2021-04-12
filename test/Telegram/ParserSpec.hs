module Telegram.ParserSpec where

import           Data.Maybe
import qualified Data.Text               as T (isPrefixOf, tail, unwords, words)
import           Telegram.Data
import           Telegram.Data.Arbitrary ()
import           Telegram.Parser
import           Test.Hspec
import           Test.Hspec.QuickCheck

chatIdSpec :: Spec
chatIdSpec =
  context "when given an Update with Message" $
    prop "extracts Message's ChatId" $
      \arbU -> case upd'message arbU of
        Nothing -> isNothing (arbU <?> chatId)
        Just m  -> (arbU <?> chatId) == Just (chat'id . msg'chat $ m)

updateIdSpec :: Spec
updateIdSpec =
  context "when given an Update" $
    prop "extracts its UpdateId" $
      \arbU -> (arbU <?> updateId) == Just (upd'update_id arbU)

callbackQuerySpec :: Spec
callbackQuerySpec =
  context "when given an Update with CallbackQuery" $
    prop "extracts this query" $
      \arbU -> (arbU <?> callbackQuery) == upd'callback_query arbU

callbackIdSpec :: Spec
callbackIdSpec =
  context "when given an Update with CallbackQuery" $
    prop "extracts it's id" $
      \arbU -> case cq'id <$> upd'callback_query arbU of
        Nothing -> isNothing (arbU <?> callbackId)
        Just i  -> (arbU <?> callbackId) == Just i

callbackDataSpec :: Spec
callbackDataSpec =
  context "when given an Update with CallbackQuery" $
    prop "extracts it's data" $
      \arbU -> case upd'callback_query arbU >>= cq'data of
        Nothing -> isNothing (arbU <?> callbackData)
        Just md -> (arbU <?> callbackData) == Just md

callbackMessageIdSpec :: Spec
callbackMessageIdSpec =
  context "when given an Update with CallbackQuery" $
    prop "extracts it's message id" $
      \arbU -> case fmap msg'message_id $ upd'callback_query arbU >>= cq'message of
        Nothing -> isNothing (arbU <?> callbackMessageId)
        Just md -> (arbU <?> callbackMessageId) == Just md

callbackChatIdSpec :: Spec
callbackChatIdSpec =
  context "when given an Update with CallbackQuery" $
    prop "extracts it's chat id" $
      \arbU -> case fmap (chat'id . msg'chat) $ upd'callback_query arbU >>= cq'message of
        Nothing -> isNothing (arbU <?> callbackChatId)
        Just md -> (arbU <?> callbackChatId) == Just md

captionSpec :: Spec
captionSpec =
  context "when given an Update with caption" $
    prop "extracts it" $
      \arbU -> case msg'caption <$> upd'message arbU of
        Nothing  -> isNothing (arbU <?> caption)
        Just cap -> (arbU <?> caption) == Just cap


textSpec :: Spec
textSpec =
  context "when given an Update with text" $
    prop "extracts text from Update" $
      \arbU -> case upd'message arbU >>= msg'text of
        Nothing -> isNothing (arbU <?> text)
        Just t  -> (arbU <?> text) == Just t

commandSpec :: Spec
commandSpec =
  context "when given an Update with command" $
    prop "returns ()" $
      \arbU arbT -> case upd'message arbU >>= msg'text of
        Nothing -> isNothing (arbU <?> command arbT)
        Just t -> if "/" `T.isPrefixOf` t
          then case T.words t of
            (w:ws) -> (arbU <?> command (T.tail w)) == Just ()
          else isNothing (arbU <?> command arbT)

stickerSpec :: Spec
stickerSpec =
  context "when given an Update with Sticker" $
    prop "extracts file_id of Sticker" $
      \arbU -> case upd'message arbU >>= msg'sticker of
        Nothing -> isNothing (arbU <?> sticker)
        Just s  -> (arbU <?> sticker) == Just (stk'file_id s)


photoSpec :: Spec
photoSpec =
  context "when given an Update with Photo" $
    prop "extracts file_id of first PhotoSize" $
      \arbU -> case upd'message arbU >>= msg'photo of
        Nothing -> isNothing (arbU <?> photo)
        Just p  -> (arbU <?> photo) == Just (ph'file_id $ head p)

animationSpec :: Spec
animationSpec =
  context "when given an Update with Animation" $
    prop "extracts file_id of Animation" $
      \arbU -> case upd'message arbU >>= msg'animation of
        Nothing -> isNothing (arbU <?> animation)
        Just a  -> (arbU <?> animation) == Just (anim'file_id a)

audioSpec :: Spec
audioSpec =
  context "when given an Update with Audio" $
    prop "extracts file_id of Audio" $
      \arbU -> case upd'message arbU >>= msg'audio of
        Nothing -> isNothing (arbU <?> audio)
        Just a  -> (arbU <?> audio) == Just (audio'file_id a)

documentSpec :: Spec
documentSpec =
  context "when given an Update with Document" $
    prop "extracts file_id of Document" $
      \arbU -> case upd'message arbU >>= msg'document of
        Nothing -> isNothing (arbU <?> document)
        Just d  -> (arbU <?> document) == Just (doc'file_id d)

videoSpec :: Spec
videoSpec =
  context "when given an Update with Video" $
    prop "extracts file_id of Video" $
      \arbU -> case upd'message arbU >>= msg'video of
        Nothing -> isNothing (arbU <?> video)
        Just v  -> (arbU <?> video) == Just (vid'file_id v)

videoNoteSpec :: Spec
videoNoteSpec =
  context "when given an Update with VideoNote" $
    prop "extracts file_id of VideoNote" $
      \arbU -> case upd'message arbU >>= msg'video_note of
        Nothing -> isNothing (arbU <?> videoNote)
        Just v  -> (arbU <?> videoNote) == Just (vidnote'file_id v)

voiceSpec :: Spec
voiceSpec =
  context "when given an Update with Voice" $
    prop "extracts file_id of Voice" $
      \arbU -> case upd'message arbU >>= msg'voice of
        Nothing -> isNothing (arbU <?> voice)
        Just v  -> (arbU <?> voice) == Just (voice'file_id v)

spec :: Spec
spec = do
  describe "chatId" chatIdSpec
  describe "updateId" updateIdSpec
  describe "callbackQuery" callbackQuerySpec
  describe "callbackId" callbackIdSpec
  describe "callbackData" callbackDataSpec
  describe "callbackMessageId" callbackMessageIdSpec
  describe "callbackChatId" callbackChatIdSpec
  describe "caption" captionSpec
  describe "text" textSpec
  describe "command" commandSpec
  describe "sticker" stickerSpec
  describe "photo" photoSpec
  describe "animation" animationSpec
  describe "audio" audioSpec
  describe "document" documentSpec
  describe "video" videoSpec
  describe "videoNote" videoNoteSpec
  describe "voice" voiceSpec
