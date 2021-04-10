{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Vk.Requests where

import           Data.Text        (Text, split, stripPrefix)
import           TH               (deriveFromJSON')
import           Vk.Data

import           Data.Aeson
import           Network.HTTP.Req (Scheme (..), Url, https, (/:))
import           Servant.API      (ToHttpApiData (..))

data GetLpsParams = GetLpsParams
    { getLps'group_id    :: Integer
    , getLps'token       :: Text
    , getLps'api_version :: Double
    }

data GetLpsResult = GetLpsResult
    { getLpsResult'response :: Maybe GetLpsResponse
    , getLpsResult'error    :: Maybe GetLpsError
    } deriving Show

data GetLpsError = GetLpsError
    { error'code :: Int
    , error'msg  :: Text
    } deriving (Show)

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

data CheckLpsResponse = CheckLpsResponse
    { checkLpsResp'ts      :: Maybe Ts
    , checkLpsResp'updates :: [Update]
    , checkLpsResp'failed  :: Maybe Int
    } deriving (Show)

data CheckLpsParams = CheckLpsParams
    { checkLps'server :: Text
    , checkLps'action :: CheckLpsAction
    , checkLps'key    :: Text
    , checkLps'wait   :: Maybe Int
    , checkLps'ts     :: Ts
    }

data CheckLpsAction
    = ACheck

instance ToHttpApiData CheckLpsAction where
    toQueryParam ACheck = "a_check"

data SendMessageParams = SendMessageParams
    { sendMsg'user_id      :: Int
    , sendMsg'message      :: Maybe Text
    , sendMsg'sticker_id   :: Maybe Int
    , sendMsg'attachments  :: [Attachment]
    , sendMsg'access_token :: Text
    , sendMsg'api_version  :: Double
    } deriving (Show)

newtype SendMessageResponse = SendMessageResponse
    { sendMsgResp'message_id :: Maybe Int
    } deriving (Show)

deriveFromJSON' ''CheckLpsResponse
deriveFromJSON' ''GetLpsResponse
deriveFromJSON' ''GetLpsError
deriveFromJSON' ''GetLpsResult
deriveFromJSON' ''SendMessageResponse
