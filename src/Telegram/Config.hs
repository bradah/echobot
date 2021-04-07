module Telegram.Config where

import           Configurator
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Data.Aeson
import           Data.Text                  (Text)
import           Https                      (Url, https, (/:))

newtype Config = Config
    { token :: Text
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "telegram" $ \o -> do
        token <- o .: "token"
        pure Config {..}

baseUrl
    :: Member (Reader Config) r
    => Eff r Url
baseUrl = do
    tok <- asks token
    pure $ https "api.telegram.org" /: "bot" <> tok

loadConfig
    :: Member Configurator r
    => Eff r Config
loadConfig = load "config.json"

{- runLoadConfig :: IO (Either FileProviderError  (Either ConfiguratorError  Config))
runLoadConfig = runM
    . runError
    . runError
    . localFileProvider
    . fileConfigurator
    $ loadConfig -}
