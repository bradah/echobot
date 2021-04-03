{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Vk.Internal.Request where

import           Bot.TH
import           Data.Text        (Text)
import           GHC.Generics
import           Vk.Internal.Data

import           Servant.API      (ToHttpApiData (..))

data GetLpsParams = GetLpsParams
    { getLpsGroupId    :: Integer
    , getLpsToken      :: Token
    , getLpsApiVersion :: Double
    }

data GetLpsResult = GetLpsResult
  { getLpsResultResponse :: Maybe GetLpsResponse
  , getLpsResultError    :: Maybe GetLpsError
  } deriving (Generic)

data GetLpsError = GetLpsError
  { errorCode :: Int
  , errorMsg  :: Text
  } deriving (Show, Generic)

data GetLpsResponse = GetLpsResponse
  { getLpsResponseKey    :: Text
  , getLpsResponseServer :: Text
  , getLpsResponseTs     :: Ts
  } deriving (Generic)

data CheckLpsResponse = CheckLpsResponse
  { checkLpsResponseTs      :: Maybe Ts
  , checkLpsResponseUpdates :: [Update]
  , checkLpsResponseFailed  :: Maybe Int
  } deriving (Show, Generic)

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

data SendMessageResponse = SendMessageResponse
    { sendMessageResponseMessageId :: Maybe Int
    } deriving (Show, Generic)

deriveFromJSON' ''CheckLpsResponse
deriveFromJSON' ''GetLpsResponse
deriveFromJSON' ''GetLpsError
deriveFromJSON' ''GetLpsResult
deriveFromJSON' ''SendMessageResponse
