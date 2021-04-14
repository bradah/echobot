{- |
Copyright:  (c) 2021 o-pascal
Maintainer: o-pascal <rtrn.0@ya.ru>

This module defines simple configuration loading effect.
-}

module Eff.Configurator
    (
    -- * Configurator
    -- ** Instruction set
      Configurator (..)
    , load
    -- ** Run
    , runPureConfigurator
    ) where

import           Control.Monad.Freer
import           Data.Aeson
import           Data.ByteString.Lazy (fromStrict)
import           Data.Text.Encoding
import           Prelude              hiding (readFile)

import           Eff.Error
import           Eff.FileProvider

-- | Instruction set for 'Configurator' effect.
data Configurator a where
    Load :: FromJSON a => FilePath -> Configurator a

-- | Load 'FromJSON' instance from 'FilePath'.
load :: ( Member Configurator r
        , FromJSON a
        )
     => FilePath
     -> Eff r a
load = send . Load

-- | Run 'Configurator' effect. Note that it actually doesn't
-- depend on IO, so one could implement 'FileProvider' using pure
-- computations to simulate config loading purely.
runPureConfigurator :: ( Members [FileProvider, Error AppError] r
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
