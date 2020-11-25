{-# LANGUAGE OverloadedStrings #-}
module VK.Config where


import qualified Data.Configurator as Conf
import           Data.Text         (Text)

type Token = Text
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
