module Configurator where

import           Control.Monad.Freer
import           Data.Aeson
import           Data.ByteString.Lazy (fromStrict)
import           Data.Text.Encoding
import           Error
import           Prelude              hiding (readFile)

import           FileProvider

data Configurator a where
    Load :: FromJSON a => FilePath -> Configurator a

load
    :: ( Member Configurator r
       , FromJSON a
       )
    => FilePath
    -> Eff r a
load = send . Load

runPureConfigurator
    :: ( Members [FileProvider, Error AppError] r
       , FromJSON a
       )
    => Eff (Configurator : r) a
    -> Eff r a
runPureConfigurator = interpret $ \case
    Load path -> do
        text <- readFile path
        let bs = fromStrict $ encodeUtf8 text
        case eitherDecode bs of
            Left e     -> throwError $ ConfiguratorError path e
            Right conf -> pure conf
