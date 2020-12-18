{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Vk.Methods.Request where

import           Data.Text        (Text)
import           GHC.Generics
import           Network.HTTP.Req
import           Vk.Types
import           Vk.Types.Derive

import           Servant.API      (ToHttpApiData (..))

type LpsServer = Text
type LpsKey = Text
type Ts = Text

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
  { getLpsResponseKey    :: LpsKey
  , getLpsResponseServer :: Text
  , getLpsResponseTs     :: Ts
  } deriving (Generic)

data CheckLpsResponse = CheckLpsResponse
  { checkLpsResponseTs      :: Maybe Ts
  , checkLpsResponseUpdates :: [Update]
  , checkLpsResponseFailed  :: Maybe Int
  } deriving (Show, Generic)

data CheckLpsParams = CheckLpsParams
    { checkLpsServer :: LpsServer
    , checkLpsAction :: CheckLpsAction
    , checkLpsKey    :: LpsKey
    , checkLpsWait   :: Maybe Int
    , checkLpsTs     :: Ts
    }

data CheckLpsAction
    = ACheck

instance ToHttpApiData CheckLpsAction where
    toQueryParam ACheck = "a_check"

data SendMessageParams = SendMessageParams
    { sendMessageUserId      :: UserId
    , sendMessageMessage     :: Text
    , sendMessageAccessToken :: Token
    , sendMessageApiVersion  :: Double
    }

data SendMessageResponse = SendMessageResponse
    { sendMessageResponseMessageId :: Maybe Int
    } deriving (Show, Generic)

deriveJSON' ''CheckLpsResponse
deriveJSON' ''GetLpsResponse
deriveJSON' ''GetLpsError
deriveJSON' ''GetLpsResult
deriveJSON' ''SendMessageResponse
