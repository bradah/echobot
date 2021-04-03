{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines Bot monad transformer.
-}

module Telegram.Internal.Bot
    ( -- * Bot monad
      -- ** Bot
      Bot(..)
      -- ** Env
    , Env(..)
    -- ** BotState
    , BotState(..)
    , initState
    ) where

import           Colog                  hiding (Message)
import qualified Colog                  (Message)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.HashMap.Lazy
import           Data.Text              (Text)
import           Servant.Client

import qualified Bot.State              as Bot
import           Bot.Utils              ()
import           Telegram.Internal.Data

-- | Base monad representing bot application.
newtype Bot a = Bot
    { runBot :: ReaderT Env (StateT BotState ClientM) a
    } deriving ( Functor, Applicative, Monad
               , MonadReader Env, MonadState BotState, MonadIO
               )

-- | Environment in which bot runs.
data Env = Env
    { envToken     :: Text
    , envLogAction :: LogAction Bot Colog.Message
    , envCleintEnv :: ClientEnv
    } deriving Show

-- | State of bot.
type BotState = Bot.BotState (Maybe Int) Message

-- | Initial bot state.
initState :: BotState
initState = Bot.BotState Nothing empty

instance HasLog Env Colog.Message Bot where
    getLogAction = envLogAction
    setLogAction a e = e { envLogAction = a }
