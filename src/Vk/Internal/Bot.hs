{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Vk.Internal.Bot where

import           Colog                hiding (Message)
import qualified Colog                (Message)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Default
import           Data.HashMap.Lazy
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import           Servant.Client

import qualified Bot.State            as Bot
import           Bot.Utils            ()
import           Vk.Internal.Data
import           Vk.Internal.Request

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
    , envLpsServer  :: Text
    , envLpsKey     :: Text
    , envInitialTs  :: Maybe Ts
    , envApiVersion :: Double
    , envLogAction  :: LogAction Bot Colog.Message
    , envClientEnv  :: ClientEnv
    } deriving Show

type BotState = Bot.BotState Ts Message

initState :: Env -> BotState
initState env = Bot.BotState ts empty
  where
    ts :: Ts
    ts = fromMaybe (error "No Ts in Env") (envInitialTs env)
