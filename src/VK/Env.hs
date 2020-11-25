{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module VK.Env where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text              as T (splitOn)
import           Network.HTTP.Req
import           VK.Config
import           VK.Methods.Request

data Env = Env
  { envToken     :: Token
  , envGroupId   :: GroupId
  , envLPSServer :: LPSServer
  , envLPSKey    :: LPSKey
  , envTs        :: Ts
  } deriving (Show)

mkEnv :: Config -> IO Env
mkEnv Config{..} = do
  lpsResult <- getLPS
  case getLPSResultResponse lpsResult of
    Nothing -> error "mkEnv: no Response"
    Just response -> return $ Env
      { envToken = token
      , envGroupId = groupId
      , envLPSServer = extractUrl response
      , envLPSKey = getLPSResponseKey response
      , envTs = getLPSResponseTs response
      }
  where
    getLPS :: IO GetLPSResult
    getLPS = runReq defaultHttpConfig $ do
      let params = "group_id" =: groupId
                  <> "access_token" =: token
                  <> "v" =: apiVersion
      resp <- req POST
                  (https "api.vk.com" /: "method" /: "groups.getLongPollServer")
                  (ReqBodyUrlEnc params)
                  jsonResponse
                  mempty
      liftIO . return $ (responseBody resp :: GetLPSResult)

    extractUrl :: GetLPSResponse -> LPSServer
    extractUrl resp = foldl (/:) (https . head $ chunks) (tail chunks)
      where
        chunks = drop 2 . T.splitOn "/" . getLPSResponseServer $ resp
