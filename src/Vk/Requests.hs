{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Vk API responses.
-}

module Vk.Requests
    ( -- * Responses.
      GetLpsResult (..)
    , GetLpsError (..)
    , GetLpsResponse (..)
    , CheckLpsResponse (..)
    , CheckLpsAction (..)
    , SendMessageResponse (..)
    ) where

import           Data.Text        (Text, split, stripPrefix)
import           TH               (deriveFromJSON')
import           Vk.Data

import           Data.Aeson
import           Network.HTTP.Req (Scheme (..), Url, https, (/:))
import           Web.HttpApiData  (ToHttpApiData (..))

-- | Result of 'Vk.Methods.getLps'.
data GetLpsResult = GetLpsResult
    { getLpsResult'response :: Maybe GetLpsResponse
    , getLpsResult'error    :: Maybe GetLpsError
    } deriving Show

-- | Information of error occured in 'Vk.Methods.getLps'.
data GetLpsError = GetLpsError
    { error'code :: Int
    , error'msg  :: Text
    } deriving (Show)

-- | Actual useful information of 'GetLpsResult'.
data GetLpsResponse = GetLpsResponse
    { getLpsResp'key    :: Text
    , getLpsResp'server :: Url 'Https
    , getLpsResp'ts     :: Ts
    } deriving Show

instance FromJSON (Url 'Https) where
    parseJSON = withText "Url" $ \s -> do
        case stripPrefix "https://" s of
            Just t -> let (p:ps) = split (== '/') t
                      in pure $ foldl (/:) (https p) ps
            Nothing -> fail "Expected \"https://\" prefix"

-- | Response on 'Vk.Methods.checkLps'.
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

-- | Response on 'Vk.Methods.sendMessage'.
newtype SendMessageResponse = SendMessageResponse
    { sendMsgResp'message_id :: Maybe Int
    } deriving (Show)

deriveFromJSON' ''CheckLpsResponse
deriveFromJSON' ''GetLpsResponse
deriveFromJSON' ''GetLpsError
deriveFromJSON' ''GetLpsResult
deriveFromJSON' ''SendMessageResponse
