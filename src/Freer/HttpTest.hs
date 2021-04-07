module Freer.HttpTest where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error

import           Network.HTTP.Req          (https, (/:))

import           Telegram.Data
import           Telegram.Requests

import           Https

telegramExample
    :: Member Https r
    => Eff r (Response [Update])
telegramExample = do
    let endpoint = https ""
    get endpoint

runReqExample
    :: IO (Either HttpsError (Response [Update]))
runReqExample = runM
    . runError
    . reqHttps
    $ telegramExample
