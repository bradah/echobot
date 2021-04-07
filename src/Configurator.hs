module Configurator where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Data.Aeson
import           Data.ByteString.Lazy      (fromStrict)
import           Data.Text                 (Text)
import           Data.Text.Encoding
import           Prelude                   hiding (readFile)

import           FileProvider

data Configurator a where
    Load :: FromJSON a => FilePath -> Configurator a

newtype ConfiguratorError = FailedParse String

load
    :: ( Member Configurator r
       , FromJSON a
       )
    => FilePath
    -> Eff r a
load = send . Load

fileConfigurator
    :: ( Members [FileProvider, Error ConfiguratorError] r
       , FromJSON a
       )
    => Eff (Configurator : r) a
    -> Eff r a
fileConfigurator = interpret $ \case
    Load path -> do
        text <- readFile path
        let bs = fromStrict $ encodeUtf8 text
        case eitherDecode bs of
            Left e     -> throwError $ FailedParse e
            Right conf -> pure conf

-------------------------------------

data Config = Config
    { token :: Text
    , api   :: Text
    } deriving Show
