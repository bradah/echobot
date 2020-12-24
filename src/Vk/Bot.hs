{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
module Vk.Bot where

import           Colog
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Configurator
import           Data.Foldable           (asum)
import           Data.Maybe              (fromJust)
import           Data.Text               (Text, pack, unlines)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          hiding (Response)

import           API.Bot.Class
import qualified Vk.Internal.Methods     as Internal
import           Vk.Internal.Request
import qualified Vk.Internal.Types       as Internal
import           Vk.UpdateParser

newtype VkBot a = VkBot
    { runVkBot :: ReaderT (Env VkBot) (StateT (Maybe Ts) ClientM) a
    } deriving (Functor, Applicative, Monad,
                MonadReader (Env VkBot), MonadState (Maybe Ts), MonadIO)

instance HasLog (Env VkBot) Message VkBot where
    getLogAction = envLogAction
    setLogAction act e = e { envLogAction = act}

instance Show (LogAction a b) where
    show _ = "LogAction"


instance Bot VkBot where
    data Env VkBot = Env
        { envToken :: Internal.Token
        , envGroupId :: Integer
        , envLpsServer :: LpsServer
        , envLpsKey :: LpsKey
        , envTs :: Maybe Ts
        , envApiVersion :: Double
        , envLogAction :: LogAction VkBot Message
        } deriving Show

    type Update VkBot = Internal.Update
    type UpdateId VkBot = Ts

    getUpdates _ = do
        logInfo "Waiting for updates..."
        botEnv <- ask
        mUid <- get
        let uid = fromJust mUid
        resp <- liftClient $ Internal.checkLps (params uid botEnv)
        modify (\_ -> checkLpsResponseTs resp)
        let ups = checkLpsResponseUpdates resp
        logInfo $ "Updates received: " <> showT (length ups)
        unless (null ups) $
            logDebug $ "Updates: " <> showT ups
        return ups
      where
        params :: Ts -> Env VkBot -> CheckLpsParams
        params uid Env{..} = CheckLpsParams
            envLpsServer
            ACheck
            envLpsKey
            (Just 25)
            uid

    getUpdateId _ = get

    handleUpdate up = do
        case updateToAction up of
            Just (Start userId)      -> sendText userId startMessage
            Just (EchoText userId t) -> sendText userId t
            -- Just (EchoSticker cid s) ->
            --   void $ runReaderT (sendSticker (SendStickerBody cid s)) env
            Nothing                  -> return ()

    mkEnv = do
        conf <- load [Required "echobot.conf.local"]
        token <- require conf "vk.token"
        groupId <- require conf "vk.group_id"
        apiVersion <- require conf "vk.api_version"
        clientEnv <- defaultClientEnv
        eitherResult <- runClientM
            (Internal.getLps $ GetLpsParams groupId token apiVersion)
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
                return $ Env
                    token
                    groupId
                    server
                    key
                    (Just ts)
                    apiVersion
                    richMessageAction

    unwrap env bot = do
        clientEnv <- defaultClientEnv
        void $ runClientM (runStateT (runReaderT (runVkBot bot) env) (envTs env)) clientEnv


defaultClientEnv :: IO ClientEnv
defaultClientEnv = mkClientEnv
    <$> newManager tlsManagerSettings
    <*> pure (BaseUrl Https "api.vk.com" 443 "/method")

liftClient :: ClientM a -> VkBot a
liftClient = VkBot . lift . lift

data Action
    = Start Internal.UserId
    -- ^ Send greeting message with instructions.
    | EchoText Internal.UserId Text
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

sendText :: Internal.UserId -> Text -> VkBot ()
sendText userId t = do
    logInfo $ "Sending text \"" <> t <> "\" to user " <> showT userId
    botEnv <- ask
    resp <- liftClient $ Internal.sendMessage (params botEnv)
    logDebug $ "Response: " <> showT resp
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

showT :: Show a => a -> Text
showT = pack . show
