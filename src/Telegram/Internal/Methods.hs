{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines Telegram API methods using Servant library.
You should use methods from 'Telegram.Methods' instead.
-}

module Telegram.Internal.Methods
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
import           Servant.Client            hiding (Response)

import           Telegram.Internal.Request
import           Telegram.Internal.Types


{- | A simple method for testing your bot's auth token.
Requires no parameters. Returns basic information about
the bot in form of a 'User' object.
-}
getMe :: ClientM (Response User)
getMe = client $ Proxy @GetMe

type GetMe = "getMe" :> Get '[JSON] (Response User)

{- | Type function to describe API.
-}
type Method (name :: Symbol) (body :: *) (respType :: *)
    = name
    :> ReqBody '[JSON] body
    :> Post '[JSON] (Response respType)

{- | Use this method to get updates from Telegram server.
Incoming updates are stored on the server until the bot
receives them either way, but they will not be kept
longer than 24 hours.

You will receive JSON-serialized 'Update' objects as a result.
-}
getUpdates :: GetUpdatesBody -> ClientM (Response [Update])
getUpdates = client $ Proxy @GetUpdates

type GetUpdates = Method "getUpdates" GetUpdatesBody [Update]

{- | Use this method to send answers to callback
queries sent from inline keyboards. The answer will be
displayed to the user as a notification at the top of
the chat screen or as an alert. On success, True is returned.
-}
answerCallbackQuery :: AnswerCallbackBody -> ClientM (Response Bool)
answerCallbackQuery = client $ Proxy @AnswerCallbackQuery

type AnswerCallbackQuery
    = Method "answerCallbackQuery" AnswerCallbackBody Bool

{- | Use this method to edit text and game messages.
On success, if the edited message is not an inline message,
the edited 'Message' is returned, otherwise True is returned.
-}
editMessageText :: EditMessageTextBody -> ClientM (Response Message)
editMessageText = client $ Proxy @EditMessageText

type EditMessageText
    = Method "editMessageText" EditMessageTextBody Message

{- | Use this method to send text messages.
On success, the sent 'Message' is returned.
-}
sendMessage :: SendMessageBody -> ClientM (Response Message)
sendMessage = client $ Proxy @SendMessage

type SendMessage = Method "sendMessage" SendMessageBody Message

{- | Use this method to send static .WEBP or animated
 .TGS stickers. On success, the sent 'Message' is returned.
-}
sendSticker :: SendStickerBody -> ClientM (Response Message)
sendSticker = client $ Proxy @SendSticker

type SendSticker = Method "sendSticker" SendStickerBody Message

{- | Use this method to send photos.
On success, the sent 'Message' is returned.
-}
sendPhoto :: SendPhotoBody -> ClientM (Response Message)
sendPhoto = client $ Proxy @SendPhoto

type SendPhoto = Method "sendPhoto" SendPhotoBody Message

{- | Use this method to send animation files.
On success, the sent 'Message' is returned.
-}
sendAnimation :: SendAnimationBody -> ClientM (Response Message)
sendAnimation = client $ Proxy @SendAnimation

type SendAnimation
    = Method "sendAnimation" SendAnimationBody Message

{- | Use this method to send audio files, if you want
Telegram clients to display them in the music player.
On success, the sent 'Message' is returned.
-}
sendAudio :: SendAudioBody -> ClientM (Response Message)
sendAudio = client $ Proxy @SendAudio

type SendAudio = Method "sendAudio" SendAudioBody Message

{- | Use this method to send general files.
On success, the sent 'Message' is returned.
-}
sendDocument :: SendDocumentBody -> ClientM (Response Message)
sendDocument = client $ Proxy @SendDocument

type SendDocument = Method "sendDocument" SendDocumentBody Message

{- | Use this method to send video files, Telegram clients
support mp4 videos (other formats may be sent as 'Document').
On success, the sent 'Message' is returned.
-}
sendVideo :: SendVideoBody -> ClientM (Response Message)
sendVideo = client $ Proxy @SendVideo

type SendVideo = Method "sendVideo" SendVideoBody Message

{- | As of v.4.0, Telegram clients support rounded square mp4
videos of up to 1 minute long. Use this method to send
video messages. On success, the sent 'Message' is returned.
-}
sendVideoNote :: SendVideoNoteBody -> ClientM (Response Message)
sendVideoNote = client $ Proxy @SendVideoNote

type SendVideoNote = Method "sendVideoNote" SendVideoNoteBody Message

{- | Use this method to send audio files, if you want Telegram clients
to display the file as a playable voice message. For this to
work, your audio must be in an .OGG file encoded with OPUS
(other formats may be sent as Audio or Document). On success,
the sent 'Message' is returned.
-}
sendVoice :: SendVoiceBody -> ClientM (Response Message)
sendVoice = client $ Proxy @SendVoice

type SendVoice = Method "sendVoice" SendVoiceBody Message
