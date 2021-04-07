module Https where

import           Control.Exception         (try)
import           Control.Monad.Freer       hiding (run)
import           Control.Monad.Freer.Error
import           Control.Monad.IO.Class
import           Data.Aeson
import           Network.HTTP.Req          hiding (Https)
import qualified Network.HTTP.Req          as Req (Scheme (..))

data Https a where
    Get
        :: (FromJSON a)
        => Url 'Req.Https
        -> Https a
    Post
        :: (FromJSON a, HttpBody b)
        => Url 'Req.Https
        -> b
        -> Https a

newtype HttpsError = HttpsError HttpException
    deriving Show

get
    :: ( Member Https r
       , FromJSON a
       )
    => Url 'Req.Https
    -> Eff r a
get = send . Get

post
    :: ( Member Https r
       , FromJSON a
       , HttpBody b
       )
    => Url Req.Https
    -> b
    -> Eff r a
post endpoint body = send $ Post endpoint body

reqHttps
    :: ( LastMember IO r
       , Member (Error HttpsError) r
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

json :: FromJSON a => a -> ReqBodyJson a
json = ReqBodyJson
