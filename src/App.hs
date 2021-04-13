{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Main application logic.
-}

module App where

import           AppState
import           Control.Monad.Freer        (Eff, Members, runM)
import           Control.Monad.Freer.Reader (runReader)
import           Control.Monad.Freer.State
import           Data.Aeson                 (FromJSON (parseJSON), withObject,
                                             withText, (.:))

import           Eff.Configurator           (Configurator, load,
                                             runPureConfigurator)
import           Eff.Console
import           Eff.Echo
import           Eff.Error
import           Eff.FileProvider           (FileProvider, runIOFileProvider)
import           Eff.Https                  (Https, runIOHttps)
import           Eff.Log                    hiding (Config (..))
import qualified Eff.Log                    as Log (Config (..))
import           Eff.Random
import           Eff.Time

import           Prelude                    hiding (log)

import qualified Telegram.Config            as Tg
import qualified Telegram.Data              as Tg
import qualified Telegram.Echo              as Tg

import qualified Vk.Config                  as Vk
import qualified Vk.Data                    as Vk
import qualified Vk.Echo                    as Vk
import qualified Vk.Methods                 as Vk
import           Vk.Requests

-- | Run app executing all effects it requires.
runApp :: IO (Either AppError ())
runApp = runM
       . runError @AppError
       . runIOFileProvider
       . runIOHttps
       . runIOTime
       . runIOConsole
       . runPureConfigurator
       $ app

-- | Main logic of application.
-- This whole computation is entirely pure since
-- it doesn't require 'IO'. So implementation is
-- hidden in the interpreters.
app :: ( Members [ Configurator
                 , FileProvider
                 , Error AppError
                 , Https
                 , Console
                 , Time
                 ] r
       , HasCallStack
       )
    => Eff r ()
app = do
    conf <- load "config.json"
    runReader (log conf) . runPureLog $ do
        logDebug $ "Loaded config :" <+> conf
        case platform conf of
            Telegram -> do
                r <- runPureTelegram (telegram conf)
                logWarning $ "Telegram bot has been stopped, last state: " <+> r
            Vk -> do
                r <- runPureVk (vk conf)
                logWarning $ "Vk bot has been stopped, last state: " <+> r

-- | Run telegram bot.
runPureTelegram :: Members [ Log
                           , Error AppError
                           , Https
                           ] r
                => Tg.Config
                -> Eff r Tg.TgState
runPureTelegram conf
    = runReader conf
    . execState Tg.defaultTgState
    . Tg.runPureEcho
    $ echo @Tg.Update

-- | Run vk bot.
runPureVk :: Members [ Log
                     , Error AppError
                     , Https
                     ] r
          => Vk.Config
          -> Eff r Vk.VkState
runPureVk conf
    = evalState conf
    . execState Vk.defaultVkState
    . runPureRandom 5
    . Vk.runPureEcho $ do
    resp <- Vk.getLps
    modify (\vkConf -> vkConf
            { Vk.checkLpsServer = pure $ getLpsResp'server resp
            , Vk.checkLpsKey = getLpsResp'key resp
            }
           )
    modify (\st -> st {st'offset = getLpsResp'ts resp})
    echo @Vk.Update

-- | Application config.
data Config = Config
    { telegram :: Tg.Config
    , vk       :: Vk.Config
    , log      :: Log.Config
    , platform :: Platform
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "App config" $ \o -> Config
        <$> o .: "telegram"
        <*> o .: "vk"
        <*> o .: "log"
        <*> o .: "platform"

-- | Available platforms
data Platform
    = Telegram
    | Vk
    deriving Show

instance FromJSON Platform where
    parseJSON = withText "Platform" $ \s ->
        case s of
        "telegram" -> pure Telegram
        "vk" -> pure Vk
        _ -> fail $ "parsing Platform failed, unexpected " ++ show s ++
                    " (expected \"telegram\" or \"vk\")"
