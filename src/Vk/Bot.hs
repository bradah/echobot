{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
module Vk.Bot where

import           Data.Foldable           (forM_)
import           Data.Text.Read          (decimal)
import           Vk.Bot.Action

import           API.Bot.Class
import           Control.Monad.Reader
import           Data.Configurator
import           Data.Text               (Text, pack)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          hiding (Response)
import qualified Vk.Methods              as Vk
import           Vk.Methods.Request
import qualified Vk.Types                as Vk

{- runBot :: Env -> IO ()
runBot env = do
  ups <- getUpdates env
  forM_ ups (handleUpdate env)
  print env
  runBot $ inc env

  where
    -- | TODO
    -- Actually Ts can be obtained from updates directily
    inc :: Env -> Env
    inc env = case decimal . envTs $ env of
      Left _       -> env
      Right (n, _) -> env {envTs = pack . show $ n + 1} -}

newtype VkBot a = VkBot
    { runVkBot :: ReaderT (Env VkBot) ClientM a
    } deriving (Functor, Applicative, Monad, MonadReader (Env VkBot), MonadIO)

instance Bot VkBot where
    data Env VkBot = Env
        { envToken :: Vk.Token
        , envGroupId :: Integer
        , envLpsServer :: LpsServer
        , envLpsKey :: LpsKey
        , envTs :: Ts
        , envApiVersion :: Double
        } deriving Show

    type Update VkBot = CheckLpsResponse
    type UpdateId VkBot = Ts

    getUpdates mUid = do
        uid <- maybe (asks envTs) pure mUid
        botEnv <- ask
        liftClient $ Vk.checkLps (params uid botEnv)
      where
        params :: Ts -> Env VkBot -> CheckLpsParams
        params uid Env{..} = CheckLpsParams
            envLpsServer
            ACheck
            envLpsKey
            (Just 25)
            uid


    mkEnv = do
        conf <- load [Required "echobot.conf.local"]
        token <- require conf "vk.token"
        groupId <- require conf "vk.group_id"
        apiVersion <- require conf "vk.api_version"
        clientEnv <- defaultClientEnv
        eitherResult <- runClientM
            (Vk.getLps $ GetLpsParams groupId token apiVersion)
            clientEnv
        let resp = either
                (error "Vk.mkEnv: bad request")
                getLpsResultResponse
                eitherResult
        case resp of
            Nothing -> error "Vk.mkEnv: successful request, but got an GetLpsError"
            Just resp -> do
                let server = getLpsResponseServer resp
                    key = getLpsResponseKey resp
                    ts = getLpsResponseTs resp
                return $ Env token groupId server key ts apiVersion


defaultClientEnv :: IO ClientEnv
defaultClientEnv = mkClientEnv
    <$> newManager tlsManagerSettings
    <*> pure (BaseUrl Https "api.vk.com" 443 "/method")

liftClient :: ClientM a -> VkBot a
liftClient = VkBot . lift
