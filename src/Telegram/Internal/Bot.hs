{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Telegram.Internal.Bot where
-- Telegram.Internal.Bot
import           Colog
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.HashMap.Lazy
import           Servant.Client

import           API.Utils               ()
import           Telegram.Internal.Types

newtype Bot a = Bot
    { runBot :: ReaderT Env (StateT BotState ClientM) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadReader Env
               , MonadState BotState
               , MonadIO
               )

data Env = Env
    { envToken     :: Token
    , envLogAction :: LogAction Bot Colog.Message
    , envCleintEnv :: ClientEnv
    } deriving Show

type ConvMap = HashMap ChatId Conversation

data BotState = BotState
    { bStateUid           :: Maybe UpdateId
    , bStateConversations :: ConvMap
    }

initState :: BotState
initState = BotState Nothing empty

data Conversation = Conversation
    { convRepeat           :: Int
    } deriving Show

changeRepeatNumber :: ChatId -> Int -> BotState -> BotState
changeRepeatNumber cid n st@(BotState _ convs) = st
    { bStateConversations =
        adjust (\(Conversation _) -> Conversation n) cid convs
    }

defaultRepeatNumber :: Int
defaultRepeatNumber = 1

instance HasLog Env Colog.Message Bot where
    getLogAction = envLogAction
    setLogAction a e = e { envLogAction = a }
