{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{- |
Copyright:  (c) 2021 o-pascal
Maintainer: o-pascal <rtrn.0@ya.ru>

Abstract DSL to work with HTTPS queries.
-}

module Eff.Https
    ( -- * Https
      -- ** Instruction Set
      Https (..)
    , Url
    , get
    , post
    , getBS
    , postMultipart
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
    , renderUrl
    ) where

import           Control.Monad.Freer                   (Eff, LastMember, Member,
                                                        interpret, send)
import           Data.Aeson                            (FromJSON (..), ToJSON,
                                                        withText)
import           Data.ByteString                       (ByteString)
import           Data.Maybe                            (fromMaybe)
import           Data.Text                             (Text, split,
                                                        stripPrefix)
import           Eff.Error                             (AppError (HttpsError),
                                                        Error, throwError, try)
import           Network.HTTP.Client                   (RequestBody (..))
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Req                      hiding (Scheme (..), Url)
import qualified Network.HTTP.Req                      as Req (Scheme (..), Url)
import           Text.URI

-- | Instruction set for 'Https' effect.
data Https a where
    Get :: (FromJSON a) => Url -> Https a
    Post :: (FromJSON a, HttpBody b) => Url -> b -> Https a
    GetBS :: Url -> Https ByteString
    PostMultipart :: (FromJSON a) => Url -> Text -> FilePath -> ByteString -> Https a

-- | Path to endpoint.
type Url = Req.Url 'Req.Https

-- | Acquire Json using HTTPS.
get :: ( Member Https r
       , FromJSON a
       )
    => Url
    -> Eff r a
get = send . Get

-- | Post request using HTTPS with Json response.
post :: ( Member Https r
        , FromJSON a
        , HttpBody b
        )
     => Url
     -> b
     -> Eff r a
post host body = send $ Post host body

-- | Acquire 'ByteString' using HTTPS.
getBS :: ( Member Https r
         )
      => Url
      -> Eff r ByteString
getBS = send . GetBS

-- | Post multipart/form-data request using HTTPS.
postMultipart :: ( Member Https r
                 , FromJSON a
                 )
              => Url
              -> Text
              -> FilePath
              -> ByteString
              -> Eff r a
postMultipart host name filePath bs =
    send $ PostMultipart host name filePath bs

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
    GetBS host ->
        run $ req GET host NoReqBody bsResponse mempty
    PostMultipart host name filePath bs -> run $ do
        let (host', options) = tryGetOptions host
        body  <- reqBodyMultipart
            [partFileRequestBody name filePath (RequestBodyBS bs)]
        req POST host' body jsonResponse options
  where
    run reqAct = do
        eitherResult <- send . try $
            responseBody <$> runReq defaultHttpConfig reqAct
        either (throwError . HttpsError) pure eitherResult

    tryGetOptions host = fromMaybe (host, mempty) $
        mkURI (renderUrl host) >>= useHttpsURI

-- | Construct url encoded parameters.
url :: FormUrlEncodedParam -> ReqBodyUrlEnc
url = ReqBodyUrlEnc

-- | Construct json request body.
json :: ToJSON a => a -> ReqBodyJson a
json = ReqBodyJson

instance FromJSON (Req.Url 'Req.Https) where
    parseJSON = withText "Url" $ \s -> do
        case stripPrefix "https://" s of
            Just t -> let (p:ps) = split (== '/') t
                      in pure $ foldl (/:) (https p) ps
            Nothing -> fail "Expected \"https://\" prefix"
