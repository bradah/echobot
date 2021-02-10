{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module contains high-level functions for Telegram API methods.
-}

module Telegram.Methods
    ( -- * Available methods
      liftClient
      -- ** Getting updates
    , getUpdates
      -- ** Callbacks
    , answerRepeatCallback
    , repeatCommand
      -- ** Updating messages
    , editMessageText
      -- ** Send text
    , sendText
      -- ** Send sticker
    , sendSticker
      -- ** Send media
    , sendPhoto
    , sendAnimation
    , sendAudio
    , sendDocument
    , sendVideo
    , sendVideoNote
    , sendVoice
    ) where

import           Control.Monad.State
import qualified Data.HashMap.Lazy         as Map
import           Data.Text                 (Text, unpack)
import           Servant.Client

import           API.Logging
import           API.Utils
import           Telegram.Internal.Bot
import qualified Telegram.Internal.Methods as Int
import           Telegram.Internal.Request
import           Telegram.Internal.Types
import           Telegram.UpdateParser

-- | Lift ClientM computation to the 'Bot' monad.
liftClient :: ClientM a -> Bot a
liftClient = Bot . lift . lift

{- | Use this method to get updates from Telegram server.
Incoming updates are stored on the server until the bot
receives them either way, but they will not be kept
longer than 24 hours.

You will receive JSON-serialized 'Update' objects as a result.
-}
getUpdates :: Bot [Update]
getUpdates = do
    logInfo "Waiting for updates..."
    body <- mkBody
    resp <- liftClient $ Int.getUpdates body
    let ups = responseResult resp
    logInfo $ "Updates received: " <!> showP (length ups)
    logDebug $ "Response: " <!> showP resp
    modify (\st -> st {bStateUid = getNewUid ups})
    conversations <- addConversations ups
    modify (\st -> st {bStateConversations = conversations})
    logDebug $ "Current conversations: " <!> showP conversations
    return ups

  where
    mkBody :: Bot GetUpdatesBody
    mkBody = do
        uid <- gets bStateUid
        return $ GetUpdatesBody uid [UpdateMessage] (Just 25)

    getNewUid :: [Update] -> Maybe UpdateId
    getNewUid ups
        | null ups = Nothing
        | otherwise = (1+) <$> updateId <?> last ups

    addConversations :: [Update] -> Bot ConvMap
    addConversations ups = do
        convMap <- gets bStateConversations
        let cIds = extractChatId ups
        return $ insertIfNew cIds convMap
      where
        insertIfNew :: [ChatId] -> ConvMap -> ConvMap
        insertIfNew [] convs = convs
        insertIfNew (c:cs) convs =
            if not $ c `Map.member` convs
                then
                    Map.insert
                        c
                        (Conversation defaultRepeatNumber)
                        convs
                else
                    insertIfNew cs convs

        extractChatId :: [Update] -> [ChatId]
        extractChatId [] = []
        extractChatId (u:us) = case updateChatId <?> u of
            Just cid -> cid : extractChatId us
            Nothing  -> extractChatId us

{- | Use this method to send answers to callback
queries sent from inline keyboards. The answer will be
displayed to the user as a notification at the top of
the chat screen or as an alert. On success, True is returned.
-}
answerCallbackQuery :: CallbackId -> Maybe Text -> Maybe Bool -> Bot ()
answerCallbackQuery cbId mt mb = do
    logInfo $ "Answering callback"
        <> showP cbId
        <> " with text "
        <> showP mt
    resp <- liftClient $ Int.answerCallbackQuery body
    logDebug $ "Response: " <!> showP resp
  where
    body = AnswerCallbackBody cbId mt mb

-- | Answer callback query issued after "\/repeat" command.
answerRepeatCallback :: ChatId -> MessageId -> CallbackId -> Text -> Bot ()
answerRepeatCallback cid mid cbid t = do
    answerCallbackQuery cbid Nothing Nothing
    let n = read $ unpack t
        newText = repeatNewText n
    editMessageText cid mid newText Nothing
    logDebug $ "Changing repetition number for "
        <> showP cid
        <> " to "
        <> showP t
    modify (changeRepeatNumber cid n)
  where
    repeatNewText :: Int -> Text
    repeatNewText n =
        "From now on I will repeat your messages "
        <> showT n
        <> " time"
        <> if n > 1 then "s" else ""

-- | Answer "\/repeat" command.
-- This function will also send inline keyboard to the user.
repeatCommand :: ChatId -> Bot ()
repeatCommand cid = do
    logInfo "Received /repeat command"
    logDebug $ "Sending inline keyboard "
        <> showP defaultRepeatInlineKeyboard
        <> " to "
        <> showP cid
    resp <- liftClient $ Int.sendMessage body
    logDebug $ "Reponse: " <!> showP resp
  where
    body = SendMessageBody cid repeatGreeting (Just defaultRepeatInlineKeyboard)

    repeatGreeting :: Text
    repeatGreeting = "Choose how many times you want me to repeat your messages"

    defaultRepeatInlineKeyboard :: InlineKeyboardMarkup
    defaultRepeatInlineKeyboard = InlineKeyboardMarkup
        [
            [ InlineKeyboardButton "1" (Just "1")
            , InlineKeyboardButton "2" (Just "2")
            , InlineKeyboardButton "3" (Just "3")
            ]
        ,
            [ InlineKeyboardButton "4" (Just "4")
            , InlineKeyboardButton "5" (Just "5")
            ]
        ]

{- | Use this method to edit text and game messages.
On success, if the edited message is not an inline message,
the edited 'Telegram.Internal.Types.Message' is returned, otherwise True is returned.
-}
editMessageText :: ChatId -> MessageId -> Text -> Maybe InlineKeyboardMarkup -> Bot ()
editMessageText cid mid t mMarkup = do
    logInfo $ "Editing message "
        <> showP mid
        <> " at chat "
        <> showP cid
        <> " to "
        <> showP t
        <> " with markup "
        <> showP mMarkup
    resp <- liftClient $ Int.editMessageText body
    logDebug $ "Response: " <!> showP resp
  where
    body = EditMessageTextBody cid mid t mMarkup

{- | Use this method to send text messages.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendText :: ChatId -> Text -> Maybe InlineKeyboardMarkup -> Bot ()
sendText cid t markup = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending text "
        <> showP t
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendMessage body)
    logDebug $ "Response: " <!> showP resp
  where
    body = SendMessageBody cid t markup

{- | Use this method to send static .WEBP or animated
 .TGS stickers. On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendSticker :: ChatId -> FileId -> Bot ()
sendSticker cid fid = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending sticker "
        <> showP fid
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendSticker body)
    logDebug $ "Response: " <!> showP resp
  where
    body = SendStickerBody cid fid

{- | Use this method to send photos.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendPhoto :: ChatId -> FileId -> Caption -> Bot ()
sendPhoto cid fid cap = sendMediaWithCaption Int.sendPhoto body "photo" cid
  where
    body = SendPhotoBody cid fid cap

{- | Use this method to send animation files.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendAnimation :: ChatId -> FileId -> Caption -> Bot ()
sendAnimation cid fid cap = sendMediaWithCaption Int.sendAnimation body "animation" cid
  where
    body = SendAnimationBody cid fid cap

{- | Use this method to send audio files, if you want
Telegram clients to display them in the music player.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendAudio :: ChatId -> FileId -> Caption -> Bot ()
sendAudio cid fid cap = sendMediaWithCaption Int.sendAudio body "audio" cid
  where
    body = SendAudioBody cid fid cap

{- | Use this method to send general files.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendDocument :: ChatId -> FileId -> Caption -> Bot ()
sendDocument cid fid cap = sendMediaWithCaption Int.sendDocument body "document" cid
  where
    body = SendDocumentBody cid fid cap

{- | Use this method to send video files, Telegram clients
support mp4 videos (other formats may be sent as 'Document').
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendVideo :: ChatId -> FileId -> Caption -> Bot ()
sendVideo cid fid cap = sendMediaWithCaption Int.sendVideo body "video" cid
  where
    body = SendVideoBody cid fid cap

{- | As of v.4.0, Telegram clients support rounded square mp4
videos of up to 1 minute long. Use this method to send
video messages. On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendVideoNote :: ChatId -> FileId -> Bot ()
sendVideoNote cid fid = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending videonote "
        <> showP fid
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ Int.sendVideoNote body)
    logDebug $ "Response " <!> showP resp
  where
    body = SendVideoNoteBody cid fid

{- | Use this method to send audio files, if you want Telegram clients
to display the file as a playable voice message. For this to
work, your audio must be in an .OGG file encoded with OPUS
(other formats may be sent as Audio or Document). On success,
the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendVoice :: ChatId -> FileId -> Caption -> Bot ()
sendVoice cid fid cap = sendMediaWithCaption Int.sendVoice body "voice" cid
  where
    body = SendVoiceBody cid fid cap

-- | Simple abstraction above all methods sending media with 'Caption'.
sendMediaWithCaption
    :: (Show response)
    => (body -> ClientM response) -- ^ Bot method
    -> body -- ^ Request body
    -> Text -- ^ Name of method (for logging)
    -> ChatId -- ^ Destination 'ChatId'
    -> Bot ()
sendMediaWithCaption method body name cid = do
    repeatNum <- convRepeat . (Map.! cid) <$> gets bStateConversations
    logInfo $ "Sending "
        <> name
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ method body)
    logDebug $ "Response " <!> showP resp
