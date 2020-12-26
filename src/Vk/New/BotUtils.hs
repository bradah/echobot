{-# LANGUAGE OverloadedStrings #-}
module Vk.New.BotUtils where

import           Colog
import           Control.Applicative     ((<|>))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Configurator
import           Data.Foldable           (asum)
import           Data.Text               (Text, unlines, unpack)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client
import           System.Directory        (doesFileExist)

import           API.Utils
import qualified Vk.Internal.Methods     as Int
import           Vk.Internal.Request
import           Vk.Internal.Types
import           Vk.New.Bot
import           Vk.New.Methods
import           Vk.UpdateParser

mkEnv :: LogAction Bot Colog.Message -> IO Env
mkEnv act = do
    localExists <- doesFileExist "echobot.conf.local"
    let path = if localExists
                then "echobot.conf.local"
                else "echobot.conf"
    conf <- load [Required path]
    token <- require conf "vk.token"
    groupId <- require conf "vk.group_id"
    apiVersion <- require conf "vk.api_version"
    clientEnv <- defaultClientEnv
    eitherResult <- runClientM
        (Int.getLps $ GetLpsParams groupId token apiVersion)
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
            pure $ Env
                token
                groupId
                server
                key
                (Just ts)
                apiVersion
                act
                clientEnv
  where
    defaultClientEnv :: IO ClientEnv
    defaultClientEnv = mkClientEnv
        <$> newManager tlsManagerSettings
        <*> pure (BaseUrl Https "api.vk.com" 443 "/method")

run :: LogAction Bot Colog.Message -> IO ()
run act = do
    env <- mkEnv act
    let botState = initState env
    void $ runClientM (runStateT (runReaderT (runBot initBot) env) botState) (envClientEnv env)

initBot :: Bot ()
initBot = do
    env <- ask
    logDebug $ "Created environment: " <> showP env
    loop

loop :: Bot ()
loop = forever $ getUpdates >>= mapM_ handleUpdate

handleUpdate :: Update -> Bot ()
handleUpdate up = do
    case updateToAction up of
        Just (Start userId)      -> sendText userId startMessage
        Just (EchoText userId t) -> sendText userId t
        -- Just (EchoSticker cid s) ->
        --   void $ runReaderT (sendSticker (SendStickerBody cid s)) env
        Nothing                  -> return ()

data Action
    = Start UserId
    -- ^ Send greeting message with instructions.
    | EchoText UserId Text
    -- ^ Echo plain text.
    --   | EchoSticker UserId FileId
    -- ^ Echo 'Sticker'.

-- | Map proper 'Action' to given 'Update'.

updateToAction :: Update -> Maybe Action
updateToAction = runUpdateParser $ asum
    [ Start <$ command "start" <*> updateUserId
    , EchoText <$> updateUserId <*> text
    --   , EchoSticker <$> updateChatId <*> sticker
    ]

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
