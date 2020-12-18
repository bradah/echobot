{-# LANGUAGE OverloadedStrings #-}
module Vk.Config where


import qualified Data.Configurator as Conf
import           Data.Text         (Text)
import           Vk.Types          (Token)

type GroupId = Int

data Config = Config
  { token      :: Token
  , groupId    :: GroupId
  , apiVersion :: Double
  } deriving (Show)

load :: IO Config
load = do
  conf <- Conf.load [Conf.Required "echobot.conf.local"]
  Config
    <$> Conf.require conf "vk.token"
    <*> Conf.require conf "vk.group_id"
    <*> Conf.require conf "vk.api_version"
