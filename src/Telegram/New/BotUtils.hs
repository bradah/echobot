{-# LANGUAGE OverloadedStrings #-}
module Telegram.New.BotUtils where

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
import           Telegram.Internal.Types
import           Telegram.New.Bot
import           Telegram.New.Methods
import           Telegram.UpdateParser

mkEnv :: LogAction Bot Colog.Message -> IO Env
mkEnv act = do
    token <- getToken
    clientEnv <- defaultClientEnv token
    return $ Env token act clientEnv
  where
    getToken :: IO Token
    getToken = do
        localExists <- doesFileExist "echobot.conf.local"
        let path = if localExists
                    then "echobot.conf.local"
                    else "echobot.conf"
        conf <- load [Required path]
        require conf "telegram.token"

    defaultClientEnv :: Token -> IO ClientEnv
    defaultClientEnv token = mkClientEnv
        <$> newManager tlsManagerSettings
        <*> pure (botBaseUrl token)

    botBaseUrl :: Token -> BaseUrl
    botBaseUrl token = BaseUrl
        Https
        "api.telegram.org"
        443
        (unpack $ "/bot" <> token)


run :: LogAction Bot Colog.Message -> IO ()
run act = do
    env <- mkEnv act
    void $ runClientM (runStateT (runReaderT (runBot initBot) env) initState) (envCleintEnv env)

initBot :: Bot ()
initBot = do
    env <- ask
    logDebug $ "Created environment: " <> showP env
    loop

loop :: Bot ()
loop = forever $ getUpdates >>= mapM_ handleUpdate

data Action
    = Help ChatId
    | Repeat ChatId
    | EchoText ChatId Text
    | EchoSticker ChatId FileId
    | AnswerRepeatCallback ChatId MessageId CallbackId Text

updateToAction :: Update -> Maybe Action
updateToAction = runUpdateParser $ asum
    [ Help <$ (command "help" <|> command "start") <*> updateChatId
    , Repeat <$ command "repeat" <*> updateChatId
    , EchoText <$> updateChatId <*> text
    , EchoSticker <$> updateChatId <*> sticker
    , AnswerRepeatCallback <$> callbackChatId <*> callbackMessageId <*> callbackId <*> callbackData
    ]

handleUpdate :: Update -> Bot ()
handleUpdate up =
    case updateToAction up of
        Just (Help cid)         -> sendText cid helpMessage Nothing
        Just (Repeat cid)        -> repeatCommand cid
        Just (EchoText cid t)    -> sendText cid t Nothing
        Just (EchoSticker cid s) -> sendSticker cid s
        Just (AnswerRepeatCallback cid mid cbid cbData) ->
            answerRepeatCallback cid mid cbid cbData
        Nothing                  -> return ()

helpMessage :: Text
helpMessage = Data.Text.unlines
    [ "Hi! This bot merely echoes your messages 🙂"
    , "Just imagine talking to a mirror..."
    , ""
    , "Supported messages:"
    , "- plain text"
    , "- stickers"
    , ""
    , "Supported commands:"
    , "- /help"
    , "- /repeat - ask me to repeat your messages from 1 to 5 times in a row"
    ]
