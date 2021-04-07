module Log where

import           Control.Monad                    (forever, unless)
import           Control.Monad.Freer
import qualified Control.Monad.Freer.Error        as Freer
import           Control.Monad.Freer.Reader
import           Data.Text                        (Text, pack)
import qualified Data.Text.IO                     as T (putStrLn)
import           Data.Time.Format                 (defaultTimeLocale,
                                                   formatTime)
import           Data.Time.LocalTime              (ZonedTime, getZonedTime)
import           Prelude                          hiding (log)
import           System.IO                        (IOMode (..))

import           FileProvider

data Log a where
    Log :: Verbosity -> Text -> Log ()

log
    :: Member Log r
    => Verbosity
    -> Text
    -> Eff r ()
log v t = send $ Log v t

ioLog
    :: Members [IO, Reader Options, FileProvider, Freer.Error FileProviderError] r
    => Eff (Log : r) a
    -> Eff r a
ioLog = interpret $ \case
    Log verb text -> do
        Options {..} <- ask
        unless (verb < globalVerbosity) $ do
            time <- send getZonedTime
            let msg = format time verb text
            case whereToLog of
                None   -> pure ()
                Stdout -> send $ T.putStrLn msg
                File   -> withFile logFilePath AppendMode (`hPutStrLn` msg)
                Both -> do
                    send $ T.putStrLn msg
                    withFile logFilePath AppendMode (`hPutStrLn` msg)


format :: ZonedTime -> Verbosity -> Text -> Text
format t v s = mconcat ["[", pack prettyTime, "]", pack $ show v, s]
  where
    prettyTime = formatTime defaultTimeLocale "%c" t

logInfo
    :: Member Log r
    => Text
    -> Eff r ()
logInfo = log Info

logDebug
    :: Member Log r
    => Text
    -> Eff r ()
logDebug = log Debug

logWarning
    :: Member Log r
    => Text
    -> Eff r ()
logWarning = log Warning

logError
    :: Member Log r
    => Text
    -> Eff r ()
logError = log Error

logExample
    :: Members [IO, FileProvider] r
    => Eff (Log : r) ()
logExample = do
    h <- openFile "echobot.log" ReadMode
    forever $ do
        str <- hGetLine h
        logDebug str
        logWarning str
        logError str
        logInfo str
        send getLine

-- TODO: get rid of this
runLogExample :: IO (Either FileProviderError ())
runLogExample = runM
    . Freer.runError
    . runReader (Options Error "freer.log" Both)
    . localFileProvider
    . ioLog
    $ logExample


-- TODO: FromJSON instance
data Options = Options
    { globalVerbosity :: Verbosity
    , logFilePath     :: FilePath
    , whereToLog      :: WhereToLog
    } deriving Show

-- TODO: FromJSON instance
data Verbosity
    = Info
    | Debug
    | Warning
    | Error
    deriving (Eq, Ord)

instance Show Verbosity where
    show Info    = "[Info]    "
    show Debug   = "[Debug]   "
    show Warning = "[Warning] "
    show Error   = "[Error]   "

data WhereToLog
    = None
    | Stdout
    | File
    | Both
    deriving Show
