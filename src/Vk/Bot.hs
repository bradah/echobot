{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
module Vk.Bot where

import           Control.Monad.State
import           Data.Configurator
import           Data.Foldable           (asum)
import           Data.Maybe              (fromJust)
import           Data.Text               (Text, unlines)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          hiding (Response)

import           API.Bot.Class
import qualified Vk.Methods              as Vk
import           Vk.Methods.Request
import qualified Vk.Types                as Vk
import           Vk.UpdateParser

newtype VkBot a = VkBot
    { runVkBot :: StateT (Env VkBot) ClientM a
    } deriving (Functor, Applicative, Monad, MonadState (Env VkBot), MonadIO)

instance Bot VkBot where
    data Env VkBot = Env
        { envToken :: Vk.Token
        , envGroupId :: Integer
        , envLpsServer :: LpsServer
        , envLpsKey :: LpsKey
        , envTs :: Maybe Ts
        , envApiVersion :: Double
        } deriving Show

    type Update VkBot = Vk.Update
    type UpdateId VkBot = Ts

    getUpdates _ = do
        botEnv <- get
        let uid = fromJust (envTs botEnv)
        resp <- liftClient $ Vk.checkLps (params uid botEnv)
        modify (\env -> env {envTs = checkLpsResponseTs resp})
        return $ checkLpsResponseUpdates resp
      where
        params :: Ts -> Env VkBot -> CheckLpsParams
        params uid Env{..} = CheckLpsParams
            envLpsServer
            ACheck
            envLpsKey
            (Just 25)
            uid

    getUpdateId _ = gets envTs

    handleUpdate up = do
        case updateToAction up of
            Just (Start userId) ->
                void $ sendText userId startMessage
            Just (EchoText userId t) ->
                void $ sendText userId t
            -- Just (EchoSticker cid s) ->
            --   void $ runReaderT (sendSticker (SendStickerBody cid s)) env
            Nothing -> return ()

    mkEnv = do
        conf <- load [Required "echobot.conf.local"]
        token <- require conf "vk.token"
        groupId <- require conf "vk.group_id"
        apiVersion <- require conf "vk.api_version"
        clientEnv <- defaultClientEnv
        eitherResult <- runClientM
            (Vk.getLps $ GetLpsParams groupId token apiVersion)
            clientEnv
        let mResp = either
                (error "Vk.mkEnv: bad request")
                getLpsResultResponse
                eitherResult
        case mResp of
            Nothing -> error "Vk.mkEnv: successful request, but got an GetLpsError"
            Just resp -> do
                let server = getLpsResponseServer resp
                    key = getLpsResponseKey resp
                    ts = getLpsResponseTs resp
                return $ Env token groupId server key (Just ts) apiVersion

    unwrap env bot = do
        clientEnv <- defaultClientEnv
        void $ runClientM (runStateT (runVkBot bot) env) clientEnv


defaultClientEnv :: IO ClientEnv
defaultClientEnv = mkClientEnv
    <$> newManager tlsManagerSettings
    <*> pure (BaseUrl Https "api.vk.com" 443 "/method")

liftClient :: ClientM a -> VkBot a
liftClient = VkBot . lift

data Action
    = Start Vk.UserId
    -- ^ Send greeting message with instructions.
    | EchoText Vk.UserId Text
    -- ^ Echo plain text.
    --   | EchoSticker UserId FileId
    -- ^ Echo 'Sticker'.

-- | Map proper 'Action' to given 'Update'.

updateToAction :: Update VkBot -> Maybe Action
updateToAction = runUpdateParser $ asum
    [ Start <$ command "start" <*> updateUserId
    , EchoText <$> updateUserId <*> text
    --   , EchoSticker <$> updateChatId <*> sticker
    ]

sendText :: Vk.UserId -> Text -> VkBot SendMessageResponse
sendText userId t = do
    botEnv <- get
    liftClient $ Vk.sendMessage (params botEnv)
  where
    params :: Env VkBot -> SendMessageParams
    params Env{..} = SendMessageParams
        userId
        t
        envToken
        envApiVersion

startMessage :: Text
startMessage = Data.Text.unlines
    [ "Hi! This bot merely echoes your messages c:"
    , ""
    , "Supported messages:"
    , "- plain text"
    , "- stickers"
    , ""
    , "Supported commands:"
    , "- /start"
    ]
