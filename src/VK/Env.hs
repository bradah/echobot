{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module VK.Env where

import           Control.Monad.IO.Class
import qualified Data.Configurator      as Conf
import           Data.Text              (Text, splitOn)
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
type LPSServer = Text
type LPSKey = Text
type Token = Text

mkEnv :: IO Env
mkEnv = do
  token <- fst <$> loadConf
  gId <- snd <$> loadConf
  lpsResult <- runReq defaultHttpConfig $ do
    let params = "group_id" =: gId
              <> "access_token" =: token
              <> "v" =: ("5.124" :: Text)
    resp <- req POST
                (https "api.vk.com"
                  /: "method"
                  /: "groups.getLongPollServer"
                )
                (ReqBodyUrlEnc params)
                jsonResponse
                mempty
    liftIO . return $ (responseBody resp)

  case getLPSResultResponse lpsResult of
    Nothing -> error "mkEnv: no Response"
    Just response -> return $ Env
      { envToken = token
      , envGroupId = gId
      , envLPSServer =
          last . splitOn "/" $ getLPSResponseServer response
      , envLPSKey = getLPSResponseKey response
      , envTs = getLPSResponseTs response
      }

loadConf :: IO (Token, GroupId)
loadConf = do
  conf <- Conf.load [Conf.Required "echobot.conf.local"]
  mbToken <- Conf.lookup conf "vk.token"
  mbGroupId <- Conf.lookup conf "vk.group_id"
  case mbToken of
    Nothing -> error "echobot.conf.local: no vk.token"
    Just token -> case mbGroupId of
      Nothing  -> error "echobot.conf.local: no vk.group_id"
      Just gId -> return (token, gId)


data GetLPSResult = GetLPSResult
  { getLPSResultResponse :: Maybe GetLPSResponse
  } deriving (Generic)

data GetLPSResponse = GetLPSResponse
  { getLPSResponseKey    :: LPSKey
  , getLPSResponseServer :: LPSServer
  , getLPSResponseTs     :: Ts
  } deriving (Generic)

deriveJSON' ''GetLPSResponse
deriveJSON' ''GetLPSResult
