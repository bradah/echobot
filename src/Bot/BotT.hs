{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Bot.BotT where

import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Servant.Client

import           Bot.State

newtype BotM env state base a = BotM
    { unBotM :: ReaderT env (StateT state base) a
    } deriving ( Functor, Applicative, Monad
               , MonadReader env, MonadState state, MonadIO
               )

instance MonadTrans (BotM env state) where
    lift = BotM . lift . lift

instance (MonadUnliftIO base, MonadUnliftIO (StateT state base)) => MonadUnliftIO (BotM env state base) where
    withRunInIO = wrappedWithRunInIO BotM unBotM
