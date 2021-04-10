module App where

import           AppState
import           Configurator               (Configurator, load,
                                             runPureConfigurator)
import           Control.Monad.Freer        (Eff, LastMember, Members, runM)
import           Control.Monad.Freer.Reader (runReader)
import           Control.Monad.Freer.State
import           Data.Aeson                 (FromJSON (parseJSON), withObject,
                                             withText, (.:))
import           Error
import           FileProvider               (FileProvider, runIOFileProvider)
import           Https                      (Https, runIOHttps)
import           Log                        hiding (Config (..))
import qualified Log                        (Config (..))

import           Echo
import           Prelude                    hiding (log)
import           Random

import qualified Telegram.Config            as Tg
import qualified Telegram.Data              as Tg
import qualified Telegram.Echo              as Tg

import qualified Vk.Config                  as Vk
import qualified Vk.Data                    as Vk
import qualified Vk.Echo                    as Vk
import qualified Vk.Methods                 as Vk
import           Vk.Requests

runApp :: IO ()
runApp = do
    r <- runM
        . runError @AppError
        . runIOFileProvider
        . runPureConfigurator
        . runIOHttps
        $ app
    either print pure r

app :: ( Members [ Configurator , FileProvider , Error AppError , Https ] r
       , LastMember IO r
       , HasCallStack
       )
    => Eff r ()
app = do
    conf <- load "config.json"
    runReader (log conf) . runIOLog $ do
        logDebug $ "Loaded config :" <+> conf
        case platform conf of
            Telegram -> do
                r <- runPureTelegram (telegram conf)
                logWarning $ "Telegram bot has been stopped, last state: " <+> r
            Vk -> do
                r <- runPureVk (vk conf)
                logWarning $ "Vk bot has been stopped, last state: " <+> r

runPureTelegram :: Members [ Log , Error AppError , Https ] r
                => Tg.Config
                -> Eff r Tg.TgState
runPureTelegram conf = runReader conf
    . execState Tg.defaultTgState
    . Tg.runPureEcho
    $ echo @Tg.Update

runPureVk :: Members [ Log , Error AppError , Https ] r
          => Vk.Config
          -> Eff r Vk.VkState
runPureVk conf = evalState conf
               . execState Vk.defaultVkState
               . runPureRandom 0
               . Vk.runPureEcho $ do
    result <- Vk.getLps
    case getLpsResult'error result of
        Just e -> throwError $ VkError e
        Nothing -> case getLpsResult'response result of
            Nothing -> do
                logError $ "No response in " <+> result
                throwError $ OtherError "No resp"
            Just resp -> do
                modify (\c ->
                    c { Vk.checkLpsServer = pure $ getLpsResp'server resp
                      , Vk.checkLpsKey = getLpsResp'key resp
                      })
                modify (\st -> st {st'offset = getLpsResp'ts resp})
                echo @Vk.Update


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
