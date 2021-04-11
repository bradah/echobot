{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Abstract DSL to work with HTTPS queries.
-}

module Eff.Https
    ( -- * Https
      -- ** Instruction Set
      Https (..)
    , Url
    , get
    , post
      -- ** Run
    , runIOHttps
      -- Construct request bodies
    , url
    , ReqBodyUrlEnc (..)
    , FormUrlEncodedParam
    , json
    , ReqBodyJson (..)
    , (/:)
    , (=:)
    , https
    ) where

import           Control.Monad.Freer (Eff, LastMember, Member, interpret, send)
import           Data.Aeson          (FromJSON, ToJSON)
import           Eff.Error           (AppError (HttpsError), Error, throwError,
                                      try)
import           Network.HTTP.Req    (FormUrlEncodedParam, GET (GET), HttpBody,
                                      NoReqBody (NoReqBody), POST (POST),
                                      ReqBodyJson (..), ReqBodyUrlEnc (..),
                                      defaultHttpConfig, https, jsonResponse,
                                      req, responseBody, runReq, (/:), (=:))
import qualified Network.HTTP.Req    as Req (Scheme (..), Url)


-- | Instruction set for 'Https' effect.
data Https a where
    Get :: (FromJSON a) => Url -> Https a
    Post :: (FromJSON a, HttpBody b) => Url -> b -> Https a

-- | Path to endpoint.
type Url = Req.Url 'Req.Https

-- | Acquire data using HTTPS.
get :: ( Member Https r
       , FromJSON a
       )
    => Url
    -> Eff r a
get = send . Get

-- | Post request using HTTPS.
post :: ( Member Https r
        , FromJSON a
        , HttpBody b
        )
     => Url
     -> b
     -> Eff r a
post host body = send $ Post host body

-- | Run 'Https' in 'IO' using @req@.
runIOHttps :: ( LastMember IO r
              , Member (Error AppError) r
              , FromJSON a
              )
           => Eff (Https : r) a
           -> Eff r a
runIOHttps = interpret $ \case
    Get host ->
        run $ req GET host NoReqBody jsonResponse mempty
    Post host body ->
        run $ req POST host body jsonResponse mempty
  where
    run reqAct = do
        eitherResult <- send . try $
            responseBody <$> runReq defaultHttpConfig reqAct
        either (throwError . HttpsError) pure eitherResult

-- | Construct url encoded parameters.
url :: FormUrlEncodedParam -> ReqBodyUrlEnc
url = ReqBodyUrlEnc

-- | Construct json request body.
json :: ToJSON a => a -> ReqBodyJson a
json = ReqBodyJson
