module App where

import           Configurator               (Configurator, load,
                                             runPureConfigurator)
import           Control.Monad.Freer        (Eff, LastMember, Members, runM)
import           Control.Monad.Freer.Reader (runReader)
import           Control.Monad.Freer.State  (execState)
import           Data.Aeson                 (FromJSON (parseJSON), withObject,
                                             withText, (.:))
import           Error                      (AppError, Error, runError)
import           FileProvider               (FileProvider, runIOFileProvider)
import           Https                      (Https, runIOHttps)
import           Log                        hiding (Config (..))
import qualified Log                        (Config (..))

import           Echo
import           Prelude                    hiding (log)
import qualified Telegram.Config            as Tg
import           Telegram.Data
import           Telegram.Echo
import qualified Telegram.State             as Tg (State, defaultState)

runApp :: IO ()
runApp = do
    r <- runM
        . runError @AppError
        . runIOFileProvider
        . runPureConfigurator
        . runIOHttps
        $ app
    either print pure r

app :: ( Members [ Configurator
                 , FileProvider
                 , Error AppError
                 , Https
                 ] r
       , LastMember IO r
       )
    => Eff r ()
app = do
    conf <- load "config.json"
    runReader (log conf) . runIOLog $ do
        logDebug $ "Loaded config " <+> conf
        case platform conf of
            Telegram -> do
                r <- runPureTelegram (telegram conf)
                logWarning $ "Telegram bot has been stopped, last state: " <+> r
            Vk -> pure ()

runPureTelegram
    :: ( Members [ Log
                 , Error AppError
                 , Https
                 ]
       ) r
    => Tg.Config
    -> Eff r Tg.State
runPureTelegram conf = runReader conf
    . execState Tg.defaultState
    . runPureEcho
    $ echo @Update

data Config = Config
    { telegram :: Tg.Config
    -- , vkConfig  :: Vk.Config
    , log      :: Log.Config
    , platform :: Platform
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "App config" $ \o -> Config
        <$> o .: "telegram"
        <*> o .: "log"
        <*> o .: "platform"

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
