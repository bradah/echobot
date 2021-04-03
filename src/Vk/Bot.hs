{-# LANGUAGE OverloadedStrings #-}
module Vk.Bot where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Configurator
import           Data.Foldable           (asum)
import           Data.Text               (Text, unlines)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client
import           System.Directory        (doesFileExist)

import           Bot.Log
import qualified Bot.Log                 as Log (Message)
import           Bot.Utils
import           Vk.Internal.Bot
import           Vk.Internal.Data
import qualified Vk.Internal.Methods     as Int
import           Vk.Internal.Request
import           Vk.Methods
import           Vk.Parser

mkEnv :: LogAction Bot Log.Message -> IO Env
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

run :: LogAction Bot Log.Message -> IO ()
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
        Just (Start userId)              -> sendMessage userId startMessage []
        Just (EchoUnsupported userId)    -> sendMessage userId unsupportedText []
        Just (EchoSticker userId sId)    -> sendSticker userId sId
        Just (EchoMessage userId t atts) -> sendMessage userId t atts
        -- Just (EchoSticker cid s) ->
        --   void $ runReaderT (sendSticker (SendStickerBody cid s)) env
        Nothing                          -> return ()

data Action
    = Start UserId
    -- ^ Send greeting message with instructions.
    | EchoUnsupported UserId
    | EchoSticker UserId StickerId
    | EchoMessage UserId Text [Attachment]
    -- ^ Echo plain text.
    -- ^ Echo 'Sticker'.

-- | Map proper 'Action' to given 'Update'.

updateToAction :: Update -> Maybe Action
updateToAction = runParser $ asum
    [ Start <$ command "start" <*> updateUserId
    , EchoUnsupported <$> updateUserId <* unsupported
    , EchoSticker <$> updateUserId <*> sticker
    , EchoMessage <$> updateUserId <*> text <*> attachments
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

unsupportedText :: Text
unsupportedText = "Sorry, VK API doesn't allow me to send messages of this type"
