{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Vk API responses.
-}

module Vk.Requests
    ( -- * Responses
      Result (..)
    , Error (..)
    , tryExtract
    , GetLpsResponse (..)
    , CheckLpsResponse (..)
    , CheckLpsAction (..)
    , GetMessagesUploadServerResponse (..)
    , UploadFileResponse (..)
    ) where

import           Data.Text           (Text)
import           TH                  (deriveFromJSON')
import           Vk.Data

import           Web.HttpApiData     (ToHttpApiData (..))

import           Control.Monad.Freer
import           Eff.Error
import           Eff.Https           (Url)
import           Eff.Log

-- | Result of Vk API methods.
data Result a = Result
    { result'response :: Maybe a
    , result'error    :: Maybe VkError
    } deriving Show

-- | Information of error occured in 'Vk.Methods.getLps'.
data VkError = VkError
    { error'error_code     :: Int
    , error'error_msg      :: Text
    , error'request_params :: [ErrorRequestParam]
    } deriving (Show)

data ErrorRequestParam = ErrorRequestParam
    { erReqParams'key   :: Text
    , erReqParams'value :: Text
    } deriving Show

-- | Extract some response from 'Result' or throw an exception.
tryExtract :: ( Members [Error AppError, Log] r
               , Show a
               )
            => Result a
            -> Eff r a
tryExtract (result'error -> Just e) = do
    logError $ "Received error: " <+> e
    throwError . OtherError $ showT e
tryExtract (result'response -> Just r) = do
    logDebug $ "Received response: " <+> r
    pure r
tryExtract result = do
    logError $ "Received neither error nor response: " <+> result
    throwError . OtherError $ showT result

-- | Response of 'Vk.Methods.getLps'.
data GetLpsResponse = GetLpsResponse
    { getLpsResp'key    :: Text
    , getLpsResp'server :: Url
    , getLpsResp'ts     :: Ts
    } deriving Show

-- | Response of 'Vk.Methods.checkLps'.
data CheckLpsResponse = CheckLpsResponse
    { checkLpsResp'ts      :: Maybe Ts
    , checkLpsResp'updates :: [Update]
    , checkLpsResp'failed  :: Maybe Int
    } deriving (Show)

-- | Action for checkLps.
data CheckLpsAction
    = ACheck

instance ToHttpApiData CheckLpsAction where
    toQueryParam ACheck = "a_check"

-- | Response of 'Vk.Methods.getMessagesUploadServer'
newtype GetMessagesUploadServerResponse = GetMessagesUploadServerResponse
    { getMsgUploadServ'upload_url :: Url
    } deriving Show

newtype UploadFileResponse = UploadFileResponse
    {uploadFileResp'file :: Text
    } deriving Show


deriveFromJSON' ''CheckLpsResponse
deriveFromJSON' ''GetLpsResponse
deriveFromJSON' ''ErrorRequestParam
deriveFromJSON' ''VkError
deriveFromJSON' ''Result
deriveFromJSON' ''GetMessagesUploadServerResponse
deriveFromJSON' ''UploadFileResponse
