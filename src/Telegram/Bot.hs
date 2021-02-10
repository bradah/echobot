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


import           Control.Applicative       ((<|>))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Configurator
import           Data.Foldable             (asum)
import           Data.Text                 (Text, unlines, unpack)
import           Network.HTTP.Client       (newManager)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Servant.Client
import           System.Directory          (doesFileExist)

import           API.Logging
import qualified API.Logging               as Log (Message)
import           API.Utils
import           Telegram.Internal.Bot
import           Telegram.Internal.Methods (getMe)
import           Telegram.Internal.Types
import           Telegram.Methods
import           Telegram.UpdateParser


-- | Make new bot environment.
mkEnv
    :: LogAction Bot Log.Message -- ^ Logging action.
    -> IO Env
mkEnv act = do
    token <- getToken
    clientEnv <- defaultClientEnv token
    return $ Env token act clientEnv
  where
    -- | Get token from config file.
    getToken :: IO Token
    getToken = do
        localExists <- doesFileExist "echobot.conf.local"
        let path = if localExists
                    then "echobot.conf.local"
                    else "echobot.conf"
        conf <- load [Required path]
        require conf "telegram.token"

    -- | Make new servant client environment.
    defaultClientEnv :: Token -> IO ClientEnv
    defaultClientEnv token = mkClientEnv
        <$> newManager tlsManagerSettings
        <*> pure (botBaseUrl token)

    -- | Construct base URL.
    botBaseUrl :: Token -> BaseUrl
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
    = Help ChatId
    -- ^ Answer "\/start" or "\/help" commands.
    | Repeat ChatId
    -- ^ Answer "\/repeat" command.
    | EchoText ChatId Text
    -- ^ Echo plain text.
    | EchoSticker ChatId FileId
    -- ^ Echo 'Sticker'.
    | EchoPhoto ChatId FileId Caption
    -- ^ Echo photos, see 'PhotoSize'.
    | EchoAnimation ChatId FileId Caption
    -- ^ Echo 'Animation'.
    | EchoAudio ChatId FileId Caption
    -- ^ Echo 'Audio'.
    | EchoDocument ChatId FileId Caption
    -- ^ Echo 'Document'.
    | EchoVideo ChatId FileId Caption
    -- ^ Echo 'Video'.
    | EchoVideoNote ChatId FileId
    -- ^ Echo 'VideoNote'.
    | EchoVoice ChatId FileId Caption
    -- ^ Echo 'Voice'.
    | AnswerRepeatCallback ChatId MessageId CallbackId Text
    -- ^ Answer 'CallbackQuery', issued by pressing 'InlineKeyboardButton'.

-- | Map proper 'Action' to an 'Update'.
updateToAction :: Update -> Maybe Action
updateToAction = runUpdateParser $ asum
    [ Help <$ (command "help" <|> command "start") <*> updateChatId
    , Repeat <$ command "repeat" <*> updateChatId
    , EchoText <$> updateChatId <*> plainText
    , EchoSticker <$> updateChatId <*> sticker
    , EchoPhoto <$> updateChatId <*> photo <*> caption
    , EchoAnimation <$> updateChatId <*> animation <*> caption
    , EchoAudio <$> updateChatId <*> audio <*> caption
    , EchoDocument <$> updateChatId <*> document <*> caption
    , EchoVideo <$> updateChatId <*> video <*> caption
    , EchoVideoNote <$> updateChatId <*> videoNote
    , EchoVoice <$> updateChatId <*> voice <*> caption
    , AnswerRepeatCallback <$> callbackChatId <*> callbackMessageId <*> callbackId <*> callbackData
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
        Nothing                  -> return ()

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
