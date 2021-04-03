{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines Bot monad transformer.
-}

module Telegram.Internal.Bot
    ( -- * Bot monad
      -- ** Bot
      Bot(..)
    , runBot
    , liftClient
      -- ** Env
    , Env(..)
    -- ** BotState
    , BotState(..)
    , IsBot
    , initState
    ) where

import           Colog                  hiding (Message)
import qualified Colog                  (Message)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.HashMap.Lazy
import           Data.Text              (Text)
import           Servant.Client

import           Bot.BotT
import qualified Bot.State              as Bot
import           Bot.Utils              ()
import           Telegram.Internal.Data

-- | Base monad representing bot application.
type Bot = BotM Env BotState ClientM

runBot :: Bot a -> Env -> BotState -> IO (Either ClientError (a, BotState))
runBot bot env st = runClientM (runStateT (runReaderT (unBotM bot) env) st) (envClientEnv env)

-- | Lift ClientM computation to the 'Bot' monad.
liftClient :: ClientM a -> Bot a
liftClient = BotM . lift . lift

type IsBot t b =
    ( HasLog Env Colog.Message (t b)
    , MonadReader Env (t b)
    , MonadState BotState (t b)
    , MonadIO (t b)
    , MonadTrans t
    )

-- | Environment in which bot runs.
data Env = Env
    { envToken     :: Text
    , envLogAction :: LogAction Bot Colog.Message
    , envClientEnv :: ClientEnv
    } deriving Show

-- | State of bot.
type BotState = Bot.BotState (Maybe Int) Message

-- | Initial bot state.
initState :: BotState
initState = Bot.BotState Nothing empty

instance HasLog Env Colog.Message Bot where
    getLogAction = envLogAction
    setLogAction a e = e { envLogAction = a }
