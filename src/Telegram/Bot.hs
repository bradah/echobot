{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines basic Telegram API types.
-}

module Telegram.Bot
    ( -- * Bot
      -- ** Starting bot
      run
    , mkEnv
      -- ** Actions
    , Action(..)
    , updateToAction
    , handleUpdate
    ) where


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

import           Bot.Log
import qualified Bot.Log                 as Log (Message)
import           Bot.Utils
import           Telegram.Internal.API   (getMe)
import           Telegram.Internal.Bot
import           Telegram.Internal.Data
import           Telegram.Methods
import           Telegram.Parser


-- | Make new bot environment.
mkEnv
    :: LogAction Bot Log.Message -- ^ Logging action.
    -> IO Env
mkEnv act = do
    token <- getToken
    clientEnv <- defaultClientEnv token
    pure $ Env token act clientEnv
  where
    -- | Get token from config file.
    getToken :: IO Text
    getToken = do
        localExists <- doesFileExist "echobot.conf.local"
        let path = if localExists
                    then "echobot.conf.local"
                    else "echobot.conf"
        conf <- load [Required path]
        require conf "telegram.token"

    -- | Make new servant client environment.
    defaultClientEnv :: Text -> IO ClientEnv
    defaultClientEnv token = mkClientEnv
        <$> newManager tlsManagerSettings
        <*> pure (botBaseUrl token)

    -- | Construct base URL.
    botBaseUrl :: Text -> BaseUrl
    botBaseUrl token = BaseUrl
        Https
        "api.telegram.org"
        443
        (unpack $ "/bot" <> token)

-- | Run bot using long polling. By default timeout is 25 seconds.
run
    :: LogAction Bot Log.Message -- ^ Logging action.
    -> IO ()
run act = do
    env <- mkEnv act
    void $ runClientM (runStateT (runReaderT (runBot initBot) env) initState) (envCleintEnv env)
  where
    -- | Initial bot action.
    initBot :: Bot ()
    initBot = do
        env <- ask
        bot <- liftClient getMe
        logInfo $ "Starting Telegram bot:"
            <!> showP bot
        logDebug $ "Created environment:\n" <> showP env
        loop
    -- | Main loop in which bot runs.
    loop :: Bot ()
    loop = forever $ getUpdates >>= mapM_ handleUpdate

-- | Available actions.
data Action
    = Help Int
    -- ^ Answer "\/start" or "\/help" commands.
    | Repeat Int
    -- ^ Answer "\/repeat" command.
    | EchoText Int Text
    -- ^ Echo plain text.
    | EchoSticker Int Text
    -- ^ Echo 'Sticker'.
    | EchoPhoto Int Text (Maybe Text)
    -- ^ Echo photos, see 'PhotoSize'.
    | EchoAnimation Int Text (Maybe Text)
    -- ^ Echo 'Animation'.
    | EchoAudio Int Text (Maybe Text)
    -- ^ Echo 'Audio'.
    | EchoDocument Int Text (Maybe Text)
    -- ^ Echo 'Document'.
    | EchoVideo Int Text (Maybe Text)
    -- ^ Echo 'Video'.
    | EchoVideoNote Int Text
    -- ^ Echo 'VideoNote'.
    | EchoVoice Int Text (Maybe Text)
    -- ^ Echo 'Voice'.
    | AnswerRepeatCallback Int Int Text Text
    -- ^ Answer 'CallbackQuery', issued by pressing 'InlineKeyboardButton'.

-- | Map proper 'Action' to an 'Update'.
updateToAction :: Update -> Maybe Action
updateToAction = runParser $ asum
    [ Help <$ (command "help" <|> command "start") <*> chatId
    , Repeat <$ command "repeat" <*> chatId
    , EchoText <$> chatId <*> text
    , EchoSticker <$> chatId <*> sticker
    , EchoPhoto <$> chatId <*> photo <*> caption
    , EchoAnimation <$> chatId <*> animation <*> caption
    , EchoAudio <$> chatId <*> audio <*> caption
    , EchoDocument <$> chatId <*> document <*> caption
    , EchoVideo <$> chatId <*> video <*> caption
    , EchoVideoNote <$> chatId <*> videoNote
    , EchoVoice <$> chatId <*> voice <*> caption
    , AnswerRepeatCallback <$> chatId <*> callbackMessageId <*> callbackId <*> callbackData
    ]

-- | Map proper Telegram method to an 'Update'.
handleUpdate :: Update -> Bot ()
handleUpdate up =
    case updateToAction up of
        Just (Help cid)         -> sendText cid helpMessage Nothing
        Just (Repeat cid)        -> repeatCommand cid
        Just (EchoText cid t)    -> sendText cid t Nothing
        Just (EchoSticker cid s) -> sendSticker cid s
        Just (EchoPhoto cid ph cap) -> sendPhoto cid ph cap
        Just (EchoAnimation cid anim cap) -> sendAnimation cid anim cap
        Just (EchoAudio cid aud cap) -> sendAudio cid aud cap
        Just (EchoDocument cid doc cap) -> sendDocument cid doc cap
        Just (EchoVideo cid vid cap) -> sendVideo cid vid cap
        Just (EchoVideoNote cid vidnote) -> sendVideoNote cid vidnote
        Just (EchoVoice cid voi cap) -> sendVoice cid voi cap
        Just (AnswerRepeatCallback cid mid cbid cbData) ->
            answerRepeatCallback cid mid cbid cbData
        Nothing                  -> pure ()

-- | Greeting message
helpMessage :: Text
helpMessage = Data.Text.unlines
    [ "Hi! I will simply repeat your messages!"
    , "Just imagine talking to a mirror..."
    , ""
    , "Supported messages:"
    , "- plain text"
    , "- stickers"
    , "- photos"
    , "- voice messages"
    , "- video"
    , "- videonotes"
    , "- music"
    , "- documents"
    , "- animation"
    , ""
    , "Supported commands:"
    , "- /help"
    , "- /repeat - ask me to repeat your messages from 1 to 5 times in a row"
    ]
