module Eff.Log
    ( Log (..)
    , HasLog
    , runIOLog
    , Config (..)
    , logInfo
    , logDebug
    , logWarning
    , logError
    , HasCallStack
    , (<+>)
    ) where

import           Control.Monad              (unless)
import           Control.Monad.Freer        (Eff, Member, Members, interpret,
                                             send)
import           Control.Monad.Freer.Reader (Reader, ask)
import           Data.Aeson                 (FromJSON (parseJSON), withObject,
                                             withText, (.:))
import           Data.Text                  (Text, pack)
import qualified Data.Text.IO               as T (putStrLn)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime        (ZonedTime, getZonedTime)
import           Eff.Error                  (AppError)
import qualified Eff.Error                  as E (Error (..))
import           GHC.Stack                  (CallStack, HasCallStack,
                                             SrcLoc (..), callStack,
                                             getCallStack, withFrozenCallStack)
import           Prelude                    hiding (log)
import           System.IO                  (IOMode (..))

import           Eff.FileProvider           (FileProvider, hPutStrLn, withFile)

data Log a where
    LogMessage :: !Message -> Log ()

data Message = Message
    { msgVerbosity :: !Verbosity
    , msgStack     :: !CallStack
    , msgText      :: !Text
    }

type HasLog (r :: [* -> *]) =
    (Member Log r, HasCallStack)

logMessage
    :: Member Log r
    => Message
    -> Eff r ()
logMessage = send . LogMessage

runIOLog
    :: Members
     [ IO
     , Reader Config
     , FileProvider
     , E.Error AppError
     ] r
    => Eff (Log : r) a
    -> Eff r a
runIOLog = interpret $ \case
    LogMessage msg@Message {..} -> do
        Config {..} <- ask
        unless (msgVerbosity < globalVerbosity) $ do
            time <- send getZonedTime
            let text = format time msg
            case whereToLog of
                None   -> pure ()
                Stdout -> send $ T.putStrLn text
                File   -> withFile logFilePath AppendMode (`hPutStrLn` text)
                Both -> do
                    send $ T.putStrLn text
                    withFile logFilePath AppendMode (`hPutStrLn` text)

format :: ZonedTime -> Message -> Text
format time Message {..} = mconcat
    [ "[", prettyTime, "] "
    , pack $ show msgVerbosity
    , msgText
    , "\n\t at [", showCallStack, "]\n"
    ]
  where
    prettyTime :: Text
    prettyTime = pack $ formatTime defaultTimeLocale "%c" time

    showCallStack :: Text
    showCallStack = case reverse $ getCallStack msgStack of
        []                             -> "<unknown loc>"
        [(name, loc)]                  -> showLoc name loc
        (callerName, _) : (_, loc) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc{..} = pack srcLocModule
        <> "." <> pack name
        <> "#" <> pack (show srcLocStartLine)


logInfo, logDebug, logWarning, logError
    :: (Member Log r, HasCallStack) => Text -> Eff r ()
logInfo = log Info
logDebug = log Debug
logWarning = log Warning
logError = log Error

log :: ( Member Log r
       , HasCallStack
       )
    => Verbosity
    -> Text
    -> Eff r ()
log msgVerbosity msgText = withFrozenCallStack $ logMessage Message
        { msgStack = callStack
        , ..
        }

data Config = Config
    { globalVerbosity :: Verbosity
    , logFilePath     :: FilePath
    , whereToLog      :: WhereToLog
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "Log" $ \o -> Config
        <$> o .: "verbosity"
        <*> o .: "file_path"
        <*> o .: "where_to_log"

data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord)

instance FromJSON Verbosity where
    parseJSON = withText "Verbosity" $ \s ->
        case s of
        "debug" -> pure Debug
        "info" -> pure Info
        "warning" -> pure Warning
        "error" -> pure Error
        _ -> fail $ "parsing Verbosity failed, unexpected " ++ show s ++
                    " (expected \"debug\", \"info\", \"warning\", or \"error\")"

instance Show Verbosity where
    show Debug   = "[Debug]   "
    show Info    = "[Info]    "
    show Warning = "[Warning] "
    show Error   = "[Error]   "

data WhereToLog
    = None
    | Stdout
    | File
    | Both
    deriving Show

instance FromJSON WhereToLog where
    parseJSON = withText "WhereToLog" $ \s ->
        case s of
        "none" -> pure None
        "stdout" -> pure Stdout
        "file" -> pure File
        "both" -> pure Both
        _ -> fail $ "parsing WhereToLog failed, unexpected " ++ show s ++
                    " (expected \"none\", \"stdout\", \"file\", or \"both\")"

(<+>) :: Show a => Text -> a -> Text
t <+> a = t <> pack (show a)
