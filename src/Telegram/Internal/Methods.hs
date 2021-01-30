{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}


module Telegram.Internal.Methods where


import           Data.Proxy
import           GHC.TypeLits
import           Servant.API
import           Servant.Client            hiding (Response)

import           Telegram.Internal.Request
import           Telegram.Internal.Types

type GetMe = "getMe" :> Get '[JSON] (Response User)

getMe :: ClientM (Response User)
getMe = client $ Proxy @GetMe

type Method (name :: Symbol) (body :: *) (respType :: *)
    = name
    :> ReqBody '[JSON] body
    :> Post '[JSON] (Response respType)

type SendMessage = Method "sendMessage" SendMessageBody Message

sendMessage :: SendMessageBody -> ClientM (Response Message)
sendMessage = client $ Proxy @SendMessage


type SendSticker = Method "sendSticker" SendStickerBody Message

sendSticker :: SendStickerBody -> ClientM (Response Message)
sendSticker = client $ Proxy @SendSticker


type GetUpdates = Method "getUpdates" GetUpdatesBody [Update]

getUpdates :: GetUpdatesBody -> ClientM (Response [Update])
getUpdates = client $ Proxy @GetUpdates


type AnswerCallbackQuery
    = Method "answerCallbackQuery" AnswerCallbackBody Bool

answerCallbackQuery :: AnswerCallbackBody -> ClientM (Response Bool)
answerCallbackQuery = client $ Proxy @AnswerCallbackQuery


type EditMessageText
    = Method "editMessageText" EditMessageTextBody Message

editMessageText :: EditMessageTextBody -> ClientM (Response Message)
editMessageText = client $ Proxy @EditMessageText


type SendPhoto = Method "sendPhoto" SendPhotoBody Message

sendPhoto :: SendPhotoBody -> ClientM (Response Message)
sendPhoto = client $ Proxy @SendPhoto


type SendAnimation
    = Method "sendAnimation" SendAnimationBody Message

sendAnimation :: SendAnimationBody -> ClientM (Response Message)
sendAnimation = client $ Proxy @SendAnimation


type SendAudio = Method "sendAudio" SendAudioBody Message

sendAudio :: SendAudioBody -> ClientM (Response Message)
sendAudio = client $ Proxy @SendAudio


type SendDocument = Method "sendDocument" SendDocumentBody Message

sendDocument :: SendDocumentBody -> ClientM (Response Message)
sendDocument = client $ Proxy @SendDocument


type SendVideo = Method "sendVideo" SendVideoBody Message

sendVideo :: SendVideoBody -> ClientM (Response Message)
sendVideo = client $ Proxy @SendVideo


type SendVideoNote = Method "sendVideoNote" SendVideoNoteBody Message

sendVideoNote :: SendVideoNoteBody -> ClientM (Response Message)
sendVideoNote = client $ Proxy @SendVideoNote


type SendVoice = Method "sendVoice" SendVoiceBody Message

sendVoice :: SendVoiceBody -> ClientM (Response Message)
sendVoice = client $ Proxy @SendVoice
