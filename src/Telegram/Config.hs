{- |
Copyright:  (c) 2021 o-pascal
Maintainer: o-pascal <rtrn.0@ya.ru>

Telegram bot configuration.
-}

module Telegram.Config
    ( -- * Config
      -- ** Telegram configuration
      Config (..)
    , baseUrl
    ) where

import           Control.Monad.Freer        (Eff, Member)
import           Control.Monad.Freer.Reader (Reader, asks)
import           Data.Aeson                 (FromJSON (parseJSON), withObject,
                                             (.:))
import           Data.Text                  (Text)
import           Eff.Https                  (Url, https, (/:))

-- | Configuration data.
data Config = Config
    { token         :: Text
    -- ^ Bot token.
    , startMessage  :: Text
    -- ^ Text to send on \/start command.
    , initialRepNum :: Int
    -- ^ Initial message repetition number.
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "telegram" $ \o -> do
        token <- o .: "token"
        startMessage <- o .: "start_message"
        initialRepNum <- o .: "initial_repeat_num"
        pure Config {..}

-- | Make base url path using bot token from environment.
baseUrl
    :: Member (Reader Config) r
    => Eff r Url
baseUrl = do
    tok <- asks token
    pure $ https "api.telegram.org" /: "bot" <> tok
