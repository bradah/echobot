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
import qualified Data.HashMap.Lazy          as Map
import           Data.Text                  (Text, unpack)
import           Servant.Client

import           Bot.Log                    hiding (Message)
import           Bot.State
import           Bot.Utils
import qualified Telegram.Internal.API      as API
import           Telegram.Internal.Bot
import           Telegram.Internal.Data
import           Telegram.Internal.Requests
import           Telegram.Parser

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
    body <- mkRequest
    resp <- liftClient $ API.getUpdates body
    let ups = resp'result resp
    logInfo $ "Updates received: " <!> showP (length ups)
    logDebug $ "Response: " <!> showP resp
    modify (\st -> st { offset = getNewOffset ups })
    sessions <- modifySessions ups
    logDebug $ "Current conversations: " <!> showP sessions
    return ups

  where
    mkRequest :: Bot GetUpdatesRequest
    mkRequest = do
        os <- gets offset
        return $ GetUpdatesRequest os [UpdateMessage] (Just 25)

    getNewOffset :: [Update] -> Maybe Int
    getNewOffset ups
        | null ups = Nothing
        | otherwise = fmap (1+) $ last ups <?> updateId

    modifySessions :: [Update] -> Bot (SessionMap Message)
    modifySessions [] = gets getSesMap
    modifySessions (u:us) = case u <?> chatId of
        Just cid -> modify (newSession cid) >> modifySessions us
        _        -> modifySessions us

{- | Use this method to send answers to callback
queries sent from inline keyboards. The answer will be
displayed to the user as a notification at the top of
the chat screen or as an alert. On success, True is returned.
-}
answerCallbackQuery
    :: Text -- ^ Callback id
    -> Maybe Text
    -> Maybe Bool
    -> Bot ()
answerCallbackQuery cbId mt mb = do
    logInfo $ "Answering callback"
        <> showP cbId
        <> " with text "
        <> showP mt
    resp <- liftClient $ API.answerCallbackQuery body
    logDebug $ "Response: " <!> showP resp
  where
    body = AnswerCallbackRequest cbId mt mb

-- | Answer callback query issued after "\/repeat" command.
answerRepeatCallback
    :: Int -- ^ Chat id
    -> Int -- ^ Message id
    -> Text -- Callback id
    -> Text
    -> Bot ()
answerRepeatCallback cid mid cbid t = do
    answerCallbackQuery cbid Nothing Nothing
    let n = read $ unpack t
        newText = repeatNewText n
    editMessageText cid mid newText Nothing
    logDebug $ "Changing repetition number for "
        <> showP cid
        <> " to "
        <> showP t
    modify (changeRepNum cid n)
  where
    repeatNewText :: Int -> Text
    repeatNewText n =
        "From now on I will repeat your messages "
        <> showT n
        <> " time"
        <> if n > 1 then "s" else ""

-- | Answer "\/repeat" command.
-- This function will also send inline keyboard to the user.
repeatCommand :: Int -> Bot ()
repeatCommand cid = do
    logInfo "Received /repeat command"
    logDebug $ "Sending inline keyboard "
        <> showP defaultRepeatInlineKeyboard
        <> " to "
        <> showP cid
    resp <- liftClient $ API.sendMessage body
    logDebug $ "Reponse: " <!> showP resp
  where
    body = SendMessageRequest cid repeatGreeting (Just defaultRepeatInlineKeyboard)

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
editMessageText
    :: Int -- ^ Chat id
    -> Int -- ^ Message id
    -> Text
    -> Maybe InlineKeyboardMarkup
    -> Bot ()
editMessageText cid mid t mMarkup = do
    logInfo $ "Editing message "
        <> showP mid
        <> " at chat "
        <> showP cid
        <> " to "
        <> showP t
        <> " with markup "
        <> showP mMarkup
    resp <- liftClient $ API.editMessageText body
    logDebug $ "Response: " <!> showP resp
  where
    body = EditMessageTextRequest cid mid t mMarkup

{- | Use this method to send text messages.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendText
    :: Int -- ^ Chat id
    -> Text
    -> Maybe InlineKeyboardMarkup
    -> Bot ()
sendText cid t markup = do
    repeatNum <- gets $ getRepNum cid
    logInfo $ "Sending text "
        <> showP t
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ API.sendMessage body)
    logDebug $ "Response: " <!> showP resp
  where
    body = SendMessageRequest cid t markup

{- | Use this method to send static .WEBP or animated
 .TGS stickers. On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendSticker
    :: Int -- ^ Chat id
    -> Text -- Sticker id
    -> Bot ()
sendSticker cid fid = do
    repeatNum <- gets $ getRepNum cid
    logInfo $ "Sending sticker "
        <> showP fid
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ API.sendSticker body)
    logDebug $ "Response: " <!> showP resp
  where
    body = SendStickerRequest cid fid

{- | Use this method to send photos.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendPhoto
    :: Int -- ^ Chat id
    -> Text -- ^ Photo id
    -> Maybe Text -- ^ Caption
    -> Bot ()
sendPhoto cid fid cap = sendMediaWithCaption API.sendPhoto body "photo" cid
  where
    body = SendPhotoRequest cid fid cap

{- | Use this method to send animation files.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendAnimation
    :: Int -- ^ Chat id
    -> Text -- ^ Animation id
    -> Maybe Text -- ^ Caption
    -> Bot ()
sendAnimation cid fid cap = sendMediaWithCaption API.sendAnimation body "animation" cid
  where
    body = SendAnimationRequest cid fid cap

{- | Use this method to send audio files, if you want
Telegram clients to display them in the music player.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendAudio
    :: Int -- ^ Chat id
    -> Text -- ^ Audio id
    -> Maybe Text -- ^ Caption
    -> Bot ()
sendAudio cid fid cap = sendMediaWithCaption API.sendAudio body "audio" cid
  where
    body = SendAudioRequest cid fid cap

{- | Use this method to send general files.
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendDocument
    :: Int -- ^ Chat id
    -> Text -- ^ Document id
    -> Maybe Text -- ^ Caption
    -> Bot ()
sendDocument cid fid cap = sendMediaWithCaption API.sendDocument body "document" cid
  where
    body = SendDocumentRequest cid fid cap

{- | Use this method to send video files, Telegram clients
support mp4 videos (other formats may be sent as 'Document').
On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendVideo
    :: Int -- ^ Chat id
    -> Text -- ^ Video id
    -> Maybe Text -- ^ Caption
    -> Bot ()
sendVideo cid fid cap = sendMediaWithCaption API.sendVideo body "video" cid
  where
    body = SendVideoRequest cid fid cap

{- | As of v.4.0, Telegram clients support rounded square mp4
videos of up to 1 minute long. Use this method to send
video messages. On success, the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendVideoNote
    :: Int -- ^ Chat id
    -> Text -- ^ VideoNote id
    -> Bot ()
sendVideoNote cid fid = do
    repeatNum <- gets $ getRepNum cid
    logInfo $ "Sending videonote "
        <> showP fid
        <> " to chat "
        <> showP cid
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ API.sendVideoNote body)
    logDebug $ "Response " <!> showP resp
  where
    body = SendVideoNoteRequest cid fid

{- | Use this method to send audio files, if you want Telegram clients
to display the file as a playable voice message. For this to
work, your audio must be in an .OGG file encoded with OPUS
(other formats may be sent as Audio or Document). On success,
the sent 'Telegram.Internal.Types.Message' is returned.
-}
sendVoice
    :: Int -- ^ Chat id
    -> Text -- ^ Voice id
    -> Maybe Text -- ^ Caption
    -> Bot ()
sendVoice cid fid cap = sendMediaWithCaption API.sendVoice body "voice" cid
  where
    body = SendVoiceRequest cid fid cap

-- | Simple abstraction above all methods sending media with 'Caption'.
sendMediaWithCaption
    :: (Show response)
    => (body -> ClientM response) -- ^ Bot method
    -> body -- ^ Request body
    -> Text -- ^ Name of method (for logging)
    -> Int -- ^ Destination 'ChatId'
    -> Bot ()
sendMediaWithCaption method body name cid = do
    repeatNum <- gets $ getRepNum cid
    logInfo $ "Sending "
        <> name
        <> " (repeat number: "
        <> showP repeatNum
        <> ")"
    resp <- head <$> replicateM repeatNum (liftClient $ method body)
    logDebug $ "Response " <!> showP resp
