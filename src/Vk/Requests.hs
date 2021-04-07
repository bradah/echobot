{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Vk.Requests where

import           Data.Text   (Text)
import           TH          (deriveFromJSON')
import           Vk.Data     (Attachment, StickerId, Token, Ts, Update, UserId)

import           Servant.API (ToHttpApiData (..))

data GetLpsParams = GetLpsParams
    { getLpsGroupId    :: Integer
    , getLpsToken      :: Token
    , getLpsApiVersion :: Double
    }

data GetLpsResult = GetLpsResult
  { getLpsResultResponse :: Maybe GetLpsResponse
  , getLpsResultError    :: Maybe GetLpsError
  }

data GetLpsError = GetLpsError
  { errorCode :: Int
  , errorMsg  :: Text
  } deriving (Show)

data GetLpsResponse = GetLpsResponse
  { getLpsResponseKey    :: Text
  , getLpsResponseServer :: Text
  , getLpsResponseTs     :: Ts
  }

data CheckLpsResponse = CheckLpsResponse
  { checkLpsResponseTs      :: Maybe Ts
  , checkLpsResponseUpdates :: [Update]
  , checkLpsResponseFailed  :: Maybe Int
  } deriving (Show)

data CheckLpsParams = CheckLpsParams
    { checkLpsServer :: Text
    , checkLpsAction :: CheckLpsAction
    , checkLpsKey    :: Text
    , checkLpsWait   :: Maybe Int
    , checkLpsTs     :: Ts
    }

data CheckLpsAction
    = ACheck

instance ToHttpApiData CheckLpsAction where
    toQueryParam ACheck = "a_check"

data SendMessageParams = SendMessageParams
    { sendMessageUserId      :: UserId
    , sendMessageMessage     :: Maybe Text
    , sendMessageStickerId   :: Maybe StickerId
    , sendMessageAttachments :: [Attachment]
    , sendMessageAccessToken :: Token
    , sendMessageApiVersion  :: Double
    } deriving (Show)

newtype SendMessageResponse = SendMessageResponse
    { sendMessageResponseMessageId :: Maybe Int
    } deriving (Show)

deriveFromJSON' ''CheckLpsResponse
deriveFromJSON' ''GetLpsResponse
deriveFromJSON' ''GetLpsError
deriveFromJSON' ''GetLpsResult
deriveFromJSON' ''SendMessageResponse
