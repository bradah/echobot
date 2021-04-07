module App where

import           Control.Monad.Freer        (Eff, LastMember, Members, runM)
import           Control.Monad.Freer.Reader (runReader)
import           Control.Monad.Freer.State  (evalState)
import           Data.Aeson                 (FromJSON (parseJSON), withObject,
                                             withText, (.:))

import           Configurator               (Configurator, fileConfigurator,
                                             load)
import           Error                      (AppError, Error, runError)
import           FileProvider               (FileProvider, localFileProvider)
import           Https                      (reqHttps)
import qualified Log

import qualified Telegram.Config            as Tg
import           Telegram.Data              (Update)
import           Telegram.Methods           (getUpdates)
import           Telegram.State             (defaultState)

runApp :: IO ()
runApp = do
    r <- runM
        . runError @AppError
        . localFileProvider
        . fileConfigurator
        $ app
    either print (pure . const ()) r

app
    :: ( Members [ Configurator
                 , FileProvider
                 , Error AppError
                 ] r
       , LastMember IO r
       )
    => Eff r [Update]
app = do
    conf <- load "config.json"
    case platform conf of
        Telegram -> runReader (logConfig conf)
            . Log.ioLog
            . evalState defaultState
            . runReader (tgConfig conf)
            . reqHttps
            $ getUpdates
        Vk -> pure []

data Config = Config
    { tgConfig  :: Tg.Config
    -- , vkConfig  :: Vk.Config
    , logConfig :: Log.Config
    , platform  :: Platform
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
