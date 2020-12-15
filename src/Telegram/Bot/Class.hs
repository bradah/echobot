{-# LANGUAGE TypeFamilies #-}

module Telegram.Bot.Class where

import           Data.Kind           (Type)
import qualified Telegram.Bot        as TG
import qualified Telegram.Bot.Action as TG
import qualified Telegram.Types      as TG
import qualified VK.Types            as VK

class Bot b where
  type Update b :: Type
  type Action b :: Type
  -- type Config b :: Type
  -- loadConf :: Config b



instance Bot (TG.BotM a) where
  type Update (TG.BotM a) = TG.Update
  type Action (TG.BotM a) = TG.Action
{-   type Config (TG.BotM a) = ()
  loadConf = () -}
