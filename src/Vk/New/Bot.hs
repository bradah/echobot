{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Vk.New.Bot where

import           Colog
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.HashMap.Lazy
import           Data.Maybe           (fromMaybe)
import           Servant.Client

import           API.Utils
import           Vk.Internal.Request
import           Vk.Internal.Types

liftClient :: ClientM a -> Bot a
liftClient = Bot . lift . lift

newtype Bot a = Bot
    { runBot :: ReaderT Env (StateT BotState ClientM) a
    } deriving (Functor, Applicative, Monad,
                MonadReader Env, MonadState BotState, MonadIO)

instance HasLog Env Colog.Message Bot where
    getLogAction = envLogAction
    setLogAction a e = e {envLogAction = a}

data Env = Env
    { envToken      :: Token
    , envGroupId    :: Integer
    , envLpsServer  :: LpsServer
    , envLpsKey     :: LpsKey
    , envInitialTs  :: Maybe Ts
    , envApiVersion :: Double
    , envLogAction  :: LogAction Bot Colog.Message
    , envClientEnv  :: ClientEnv
    } deriving Show

data BotState = BotState
    { bStateTs            :: Ts
    , bStateConversations :: ConvMap
    } deriving Show

type ConvMap = HashMap UserId Conversation

data Conversation = Conversation
    { convRepeatNum :: Int
    } deriving Show

defaultRepeatNumber :: Int
defaultRepeatNumber = 1

initState :: Env -> BotState
initState env = BotState ts empty
  where
    ts :: Ts
    ts = fromMaybe (error "No Ts in Env") (envInitialTs env)
