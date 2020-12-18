{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Vk.Env where

import           Control.Monad.IO.Class
import qualified Data.Text              as T (splitOn)
import           Network.HTTP.Req
import           Vk.Config
import           Vk.Methods.Request
import           Vk.Types               (Token)

data Env = Env
  { envToken      :: Token
  , envGroupId    :: GroupId
  , envLPSServer  :: LpsServer
  , envLPSKey     :: LpsKey
  , envTs         :: Ts
  , envApiVersion :: Double
  } deriving (Show)

{- mkEnv :: Config -> IO Env
mkEnv Config{..} = do
  lpsResult <- getLPS
  case getLpsResultResponse lpsResult of
    Nothing -> error "mkEnv: no Response"
    Just response -> return $ Env
      { envToken = token
      , envGroupId = groupId
      , envLPSServer = extractUrl response
      , envLPSKey = getLpsResponseKey response
      , envTs = getLpsResponseTs response
      , envApiVersion = apiVersion
      }
  where
    getLPS :: IO GetLpsResult
    getLPS = runReq defaultHttpConfig $ do
      let params = "group_id" =: groupId
                  <> "access_token" =: token
                  <> "v" =: apiVersion
      resp <- req POST
                  (https "api.vk.com" /: "method" /: "groups.getLongPollServer")
                  (ReqBodyUrlEnc params)
                  jsonResponse
                  mempty
      liftIO . return $ (responseBody resp :: GetLpsResult)

extractUrl :: GetLpsResponse -> LpsServer
extractUrl resp = foldl (/:) (https . head $ chunks) (tail chunks)
  where
    chunks = drop 2 . T.splitOn "/" . getLpsResponseServer $ resp

-}
