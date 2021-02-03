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
      -- ** ConvMap
    , ConvMap
      -- ** Conversation
    , Conversation(..)
    , changeRepeatNumber
    , defaultRepeatNumber
    ) where

import           Colog
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.HashMap.Lazy
import           Servant.Client

import           API.Utils               ()
import           Telegram.Internal.Types

-- | Base monad representing bot application.
newtype Bot a = Bot
    { runBot :: ReaderT Env (StateT BotState ClientM) a
    } deriving ( Functor, Applicative, Monad
               , MonadReader Env, MonadState BotState, MonadIO
               )

-- | Environment in which bot runs.
data Env = Env
    { envToken     :: Token
    , envLogAction :: LogAction Bot Colog.Message
    , envCleintEnv :: ClientEnv
    } deriving Show

-- | Current bot conversations.
type ConvMap = HashMap ChatId Conversation

-- | State of bot.
data BotState = BotState
    { bStateUid           :: Maybe UpdateId
    , bStateConversations :: ConvMap
    }

-- | Initial bot state.
initState :: BotState
initState = BotState Nothing empty

-- | Bot conversation.
data Conversation = Conversation
    { convRepeat           :: Int
    } deriving Show

-- | Change number of repetitions of single message for 'Conversation'.
changeRepeatNumber :: ChatId -> Int -> BotState -> BotState
changeRepeatNumber cid n st@(BotState _ convs) = st
    { bStateConversations =
        adjust (\(Conversation _) -> Conversation n) cid convs
    }

-- | Initial repetition number.
defaultRepeatNumber :: Int
defaultRepeatNumber = 1

instance HasLog Env Colog.Message Bot where
    getLogAction = envLogAction
    setLogAction a e = e { envLogAction = a }
