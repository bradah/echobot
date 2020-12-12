{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}


module Telegram.ServMethods where


import           Data.Proxy
import           Servant.API
import           Servant.Client           hiding (Response)

import           Telegram.Methods.Request
import           Telegram.Types

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
