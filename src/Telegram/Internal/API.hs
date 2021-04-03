{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines Telegram API methods using Servant library.
You should use methods from 'Telegram.Methods' instead.
-}

module Telegram.Internal.API
    (
      -- * Telegram API methods
      -- ** getMe
      getMe
      -- ** getUpdates
    , getUpdates
      -- ** answerCallbackQuery
    , answerCallbackQuery
      -- ** editMessageText
    , editMessageText
      -- ** sendMessage
    , sendMessage
      -- ** Send media
    , sendSticker
    , sendPhoto
    , sendAnimation
    , sendAudio
    , sendDocument
    , sendVideo
    , sendVideoNote
    , sendVoice
    ) where

import           Data.Proxy
import           GHC.TypeLits
import           Servant.API
import           Servant.Client             hiding (Response)

import           Telegram.Internal.Data
import           Telegram.Internal.Requests

type TelegramAPI =
         "getMe" :> Get '[JSON] (Response User)
    :<|> "getUpdates"
         :> ReqBody '[JSON] GetUpdatesRequest
         :> Post '[JSON] (Response [Update])
    :<|> "answerCallbackQuery"
         :> ReqBody '[JSON] AnswerCallbackRequest
         :> Post '[JSON] (Response Bool)
    :<|> "editMessageText"
         :> ReqBody '[JSON] EditMessageTextRequest
         :> Post '[JSON] (Response Message)
    :<|> "sendMessage"
         :> ReqBody '[JSON] SendMessageRequest
         :> Post '[JSON] (Response Message)
    :<|> "sendSticker"
         :> ReqBody '[JSON] SendStickerRequest
         :> Post '[JSON] (Response Message)
    :<|> "sendPhoto"
         :> ReqBody '[JSON] SendPhotoRequest
         :> Post '[JSON] (Response Message)
    :<|> "sendAnimation"
         :> ReqBody '[JSON] SendAnimationRequest
         :> Post '[JSON] (Response Message)
    :<|> "sendAudio"
         :> ReqBody '[JSON] SendAudioRequest
         :> Post '[JSON] (Response Message)
    :<|> "sendDocument"
         :> ReqBody '[JSON] SendDocumentRequest
         :> Post '[JSON] (Response Message)
    :<|> "sendVideo"
         :> ReqBody '[JSON] SendVideoRequest
         :> Post '[JSON] (Response Message)
    :<|> "sendVideoNote"
         :> ReqBody '[JSON] SendVideoNoteRequest
         :> Post '[JSON] (Response Message)
    :<|> "sendVoice"
         :> ReqBody '[JSON] SendVoiceRequest
         :> Post '[JSON] (Response Message)

telegramApi :: Proxy TelegramAPI
telegramApi = Proxy

{- | A simple method for testing your bot's auth token.
Requires no parameters. Returns basic information about
the bot in form of a 'User' object.
-}
getMe :: ClientM (Response User)

{- | Use this method to get updates from Telegram server.
Incoming updates are stored on the server until the bot
receives them either way, but they will not be kept
longer than 24 hours.

You will receive JSON-serialized 'Update' objects as a result.
-}
getUpdates :: GetUpdatesRequest -> ClientM (Response [Update])

{- | Use this method to send answers to callback
queries sent from inline keyboards. The answer will be
displayed to the user as a notification at the top of
the chat screen or as an alert. On success, True is returned.
-}
answerCallbackQuery :: AnswerCallbackRequest -> ClientM (Response Bool)

{- | Use this method to edit text and game messages.
On success, if the edited message is not an inline message,
the edited 'Message' is returned, otherwise True is returned.
-}
editMessageText :: EditMessageTextRequest -> ClientM (Response Message)

{- | Use this method to send text messages.
On success, the sent 'Message' is returned.
-}
sendMessage :: SendMessageRequest -> ClientM (Response Message)

{- | Use this method to send static .WEBP or animated
 .TGS stickers. On success, the sent 'Message' is returned.
-}
sendSticker :: SendStickerRequest -> ClientM (Response Message)

{- | Use this method to send photos.
On success, the sent 'Message' is returned.
-}
sendPhoto :: SendPhotoRequest -> ClientM (Response Message)

{- | Use this method to send animation files.
On success, the sent 'Message' is returned.
-}
sendAnimation :: SendAnimationRequest -> ClientM (Response Message)

{- | Use this method to send audio files, if you want
Telegram clients to display them in the music player.
On success, the sent 'Message' is returned.
-}
sendAudio :: SendAudioRequest -> ClientM (Response Message)

{- | Use this method to send general files.
On success, the sent 'Message' is returned.
-}
sendDocument :: SendDocumentRequest -> ClientM (Response Message)

{- | Use this method to send video files, Telegram clients
support mp4 videos (other formats may be sent as 'Document').
On success, the sent 'Message' is returned.
-}
sendVideo :: SendVideoRequest -> ClientM (Response Message)

{- | As of v.4.0, Telegram clients support rounded square mp4
videos of up to 1 minute long. Use this method to send
video messages. On success, the sent 'Message' is returned.
-}
sendVideoNote :: SendVideoNoteRequest -> ClientM (Response Message)

{- | Use this method to send audio files, if you want Telegram clients
to display the file as a playable voice message. For this to
work, your audio must be in an .OGG file encoded with OPUS
(other formats may be sent as Audio or Document). On success,
the sent 'Message' is returned.
-}
sendVoice :: SendVoiceRequest -> ClientM (Response Message)

getMe
    :<|> getUpdates
    :<|> answerCallbackQuery
    :<|> editMessageText
    :<|> sendMessage
    :<|> sendSticker
    :<|> sendPhoto
    :<|> sendAnimation
    :<|> sendAudio
    :<|> sendDocument
    :<|> sendVideo
    :<|> sendVideoNote
    :<|> sendVoice
        = client telegramApi
