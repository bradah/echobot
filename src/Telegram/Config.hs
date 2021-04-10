module Telegram.Config where

import           Control.Monad.Freer        (Eff, Member)
import           Control.Monad.Freer.Reader (Reader, asks)
import           Data.Aeson                 (FromJSON (parseJSON), withObject,
                                             (.:))
import           Data.Text                  (Text)
import           Eff.Https                  (Url, https, (/:))

data Config = Config
    { token         :: Text
    , startMessage  :: Text
    , initialRepNum :: Int
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "telegram" $ \o -> do
        token <- o .: "token"
        startMessage <- o .: "start_message"
        initialRepNum <- o .: "initial_repeat_num"
        pure Config {..}

baseUrl
    :: Member (Reader Config) r
    => Eff r Url
baseUrl = do
    tok <- asks token
    pure $ https "api.telegram.org" /: "bot" <> tok
