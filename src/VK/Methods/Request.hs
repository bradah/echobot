{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module VK.Methods.Request where

import           Data.Text        (Text)
import           GHC.Generics
import           Network.HTTP.Req
import           VK.Types
import           VK.Types.Derive

type LPSServer = Url 'Https
type LPSKey = Text
type Ts = Text

data GetLPSResult = GetLPSResult
  { getLPSResultResponse :: Maybe GetLPSResponse
  , getLPSResultError    :: Maybe GetLPSError
  } deriving (Generic)

data GetLPSError = GetLPSError
  { errorCode :: Int
  , errorMsg  :: Text
  }

data GetLPSResponse = GetLPSResponse
  { getLPSResponseKey    :: LPSKey
  , getLPSResponseServer :: Text
  , getLPSResponseTs     :: Ts
  } deriving (Generic)

data Response = Response
  { responseTs      :: Maybe Ts
  , responseUpdates :: [Update]
  , responseFailed  :: Maybe Int
  } deriving (Show, Generic)

deriveJSON' ''Response
deriveJSON' ''GetLPSResponse
deriveJSON' ''GetLPSError
deriveJSON' ''GetLPSResult
