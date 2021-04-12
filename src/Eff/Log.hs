{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Simple logging DSL.
-}

module Eff.Log
    ( -- * Log
      -- ** Instruction set
      Log (..)
      -- ** Run
    , runPureLog
      -- ** Configure logging
    , Config (..)
      -- ** Log with verbosity
    , logInfo
    , logDebug
    , logWarning
    , logError
      -- ** Construct log messages
    , (<+>)
    , showT
      -- ** Find location
    , HasCallStack
    ) where

import           Control.Monad              (unless)
import           Control.Monad.Freer        (Eff, Member, Members, interpret,
                                             send)
import           Control.Monad.Freer.Reader (Reader, ask)
import           Data.Aeson                 (FromJSON (parseJSON), withObject,
                                             withText, (.:))
import           Data.Text                  (Text, pack)
import           Eff.Console
import           Eff.Error                  (AppError)
import qualified Eff.Error                  as E (Error (..))
import           Eff.Time
import           GHC.Stack                  (CallStack, HasCallStack,
                                             SrcLoc (..), callStack,
                                             getCallStack, withFrozenCallStack)
import           Prelude                    hiding (appendFile, log)

import           Eff.FileProvider


-- | Instruction set for 'Log' effect.
data Log a where
    LogMessage :: !Message -> Log ()

-- | Log message data structure.
data Message = Message
    { msgVerbosity :: !Verbosity
    , msgStack     :: !CallStack
    , msgText      :: !Text
    }

-- | Log 'Message'.
logMessage
    :: Member Log r
    => Message
    -> Eff r ()
logMessage = send . LogMessage

-- | Run 'Log' effect. This can be done purely
-- since it depends on 'Time', 'Console' and 'FileProvider'
-- effects and not on 'IO' directly.
runPureLog
    :: Members
     [ Console
     , Time
     , Reader Config
     , FileProvider
     , E.Error AppError
     ] r
    => Eff (Log : r) a
    -> Eff r a
runPureLog = interpret $ \case
    LogMessage msg@Message {..} -> do
        Config {..} <- ask
        unless (msgVerbosity < globalVerbosity) $ do
            time <- getZonedTime
            let text = format time msg
            case whereToLog of
                None   -> pure ()
                Stdout -> putLine text
                File   -> appendFile logFilePath text
                Both   -> putLine text >> appendFile logFilePath text

-- | Format messages before logging them.
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
-- | Log message with 'Info' verbosity.
logInfo = log Info
-- | Log message with 'Debug' verbosity.
logDebug = log Debug
-- | Log message with 'Warning' verbosity.
logWarning = log Warning
-- | Log message with 'Error' verbosity.
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

-- | 'Log' configuration.
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

-- | 'Log' verbosity levels.
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

-- | Destination points for log messages.
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

-- | Convert rhs value to 'Text' and concatenate with lhs.
(<+>) :: Show a => Text -> a -> Text
t <+> a = t <> showT a

-- | 'show' for 'Text'.
showT :: Show a => a -> Text
showT = pack . show
