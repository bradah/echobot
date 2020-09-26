{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module VK.Env where

import           Control.Monad.IO.Class
import           Data.Int               (Int32)
import           Data.Text              (Text)
import           GHC.Generics
import           Network.HTTP.Req
import           VK.Methods.Request
import           VK.Types.Derive

data Env = Env
  { envToken     :: Text
  , envGroupId   :: GroupId
  , envLPSServer :: LPSServer
  , envLPSKey    :: LPSKey
  , envTs        :: Ts
  } deriving (Show)

type GroupId = Text
type LPSServer = Url 'Https
type LPSKey = Text
type Token = Text

mkEnv :: Token -> GroupId -> IO Env
mkEnv token gId = do
  lpsResult <- runReq defaultHttpConfig $ do
    let params = "group_id" =: gId
              <> "access_token" =: token
              <> "v" =: ("5.124" :: Text)
    resp <- req POST
                (https "api.vk.com" /: "method" /: "groups.getLongPollServer")
                (ReqBodyUrlEnc params)
                jsonResponse
                mempty
    liftIO . return $ (responseBody resp :: GetLPSResult)

  case getLPSResultResponse lpsResult of
    Nothing -> error "mkEnv: no Response"
    Just response -> return $ Env
      { envToken = token
      , envGroupId = gId
      , envLPSServer = https $ getLPSResponseServer response
      , envLPSKey = getLPSResponseKey response
      , envTs = getLPSResponseTs response
      }

data GetLPSResult =
  GetLPSResult
  { getLPSResultResponse :: Maybe GetLPSResponse
  } deriving (Generic)

data GetLPSResponse = GetLPSResponse
  { getLPSResponseKey    :: LPSKey
  , getLPSResponseServer :: Text
  , getLPSResponseTs     :: Ts
  } deriving (Generic)

deriveJSON' ''GetLPSResponse
deriveJSON' ''GetLPSResult
