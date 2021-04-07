module Https
    ( Https (..)
    , Url
    , get
    , post
    , reqHttps
    , url
    , ReqBodyUrlEnc (..)
    , json
    , ReqBodyJson (..)
    , (/:)
    , (=:)
    , https
    ) where

import           Control.Monad.Freer (Eff, LastMember, Member, interpret, send)
import           Data.Aeson          (FromJSON, ToJSON)
import           Error               (AppError (HttpsError), Error, throwError,
                                      try)
import           Network.HTTP.Req    (FormUrlEncodedParam, GET (GET), HttpBody,
                                      NoReqBody (NoReqBody), POST (POST),
                                      ReqBodyJson (..), ReqBodyUrlEnc (..),
                                      defaultHttpConfig, https, jsonResponse,
                                      req, responseBody, runReq, (/:), (=:))
import qualified Network.HTTP.Req    as Req (Scheme (..), Url)

data Https a where
    Get
        :: (FromJSON a)
        => Url
        -> Https a
    Post
        :: (FromJSON a, HttpBody b)
        => Url
        -> b
        -> Https a

type Url = Req.Url 'Req.Https

get
    :: ( Member Https r
       , FromJSON a
       )
    => Url
    -> Eff r a
get = send . Get

post
    :: ( Member Https r
       , FromJSON a
       , HttpBody b
       )
    => Url
    -> b
    -> Eff r a
post endpoint body = send $ Post endpoint body

reqHttps
    :: ( LastMember IO r
       , Member (Error AppError) r
       , FromJSON a
       )
    => Eff (Https : r) a
    -> Eff r a
reqHttps = interpret $ \case
    Get endpoint ->
        run $ req GET endpoint NoReqBody jsonResponse mempty
    Post endpoint body ->
        run $ req POST endpoint body jsonResponse mempty
  where
    run reqAct = do
        eitherResult <- send . try $
            responseBody <$> runReq defaultHttpConfig reqAct
        either (throwError . HttpsError) pure eitherResult

url :: FormUrlEncodedParam -> ReqBodyUrlEnc
url = ReqBodyUrlEnc

json :: ToJSON a => a -> ReqBodyJson a
json = ReqBodyJson
