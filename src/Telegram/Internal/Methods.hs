{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}


module Telegram.Internal.Methods where


import           Data.Proxy
import           Servant.API
import           Servant.Client            hiding (Response)

import           Telegram.Internal.Request
import           Telegram.Internal.Types

type GetMe = "getMe" :> Get '[JSON] (Response User)

getMe :: ClientM (Response User)
getMe = client $ Proxy @GetMe


type SendMessage = "sendMessage"
    :> ReqBody '[JSON] SendMessageBody
    :> Post '[JSON] (Response Message)

sendMessage :: SendMessageBody -> ClientM (Response Message)
sendMessage = client $ Proxy @SendMessage


type SendSticker = "sendSticker"
    :> ReqBody '[JSON] SendStickerBody
    :> Post '[JSON] (Response Message)

sendSticker :: SendStickerBody -> ClientM (Response Message)
sendSticker = client $ Proxy @SendSticker


type GetUpdates = "getUpdates"
    :> ReqBody '[JSON] GetUpdatesBody
    :> Post '[JSON] (Response [Update])

getUpdates :: GetUpdatesBody -> ClientM (Response [Update])
getUpdates = client $ Proxy @GetUpdates


type AnswerCallbackQuery = "answerCallbackQuery"
    :> ReqBody '[JSON] AnswerCallbackBody
    :> Post '[JSON] (Response Bool)

answerCallbackQuery :: AnswerCallbackBody -> ClientM (Response Bool)
answerCallbackQuery = client $ Proxy @AnswerCallbackQuery


type EditMessageText = "editMessageText"
    :> ReqBody '[JSON] EditMessageTextBody
    :> Post '[JSON] (Response Message)

editMessageText :: EditMessageTextBody -> ClientM (Response Message)
editMessageText = client $ Proxy @EditMessageText


type SendPhoto = "sendPhoto"
    :> ReqBody '[JSON] SendPhotoBody
    :> Post '[JSON] (Response Message)

sendPhoto :: SendPhotoBody -> ClientM (Response Message)
sendPhoto = client $ Proxy @SendPhoto
