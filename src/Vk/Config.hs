{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Vk bot configuration.
-}

module Vk.Config
    ( -- * Config
      -- ** Vk configuration
      Config (..)
    ) where

import           Data.Aeson
import           Data.Text
import           Eff.Https  (Url)

-- | Configuration data.
data Config = Config
    { token          :: Text
    -- ^ Bot token.
    , groupId        :: Int
    -- ^ Id of a group bot associated with.
    , startMessage   :: Text
    -- ^ Text to send on \/start command.
    , initialRepNum  :: Int
    -- ^ Initial message repetition number.
    , checkLpsServer :: Maybe Url
    -- ^ Server for 'Vk.Methods.checkLps' method.
    -- It can be acquired via 'Vk.Methods.getLps'.
    , checkLpsKey    :: Text
    -- ^ Key for 'Vk.Methods.checkLps' method.
    -- Can be acquired via 'Vk.Methods.getLps'.
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "vk" $ \o -> do
        token <- o .: "token"
        groupId <- o .: "group_id"
        startMessage <- o .: "start_message"
        initialRepNum <- o .: "initial_repeat_num"
        let checkLpsServer = Nothing
            checkLpsKey = ""
        pure Config {..}
