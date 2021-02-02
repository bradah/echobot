module API.Logging where

import           Colog
import           Control.Monad.IO.Class
import           System.IO

logStdOutAndFile :: MonadIO m => Severity -> Handle -> LogAction m Message
logStdOutAndFile sev handle = cfilter ((>= sev) . msgSeverity) (richMessageAction <> richMessageToFileAction)
  where
    richMessageToFileAction :: MonadIO m => LogAction m Message
    richMessageToFileAction = upgradeMessageAction defaultFieldMap $
        cmapM fmtRichMessageDefault (logTextHandle handle)

logStdOut :: MonadIO m => Severity -> LogAction m Message
logStdOut sev = cfilter ((>= sev) . msgSeverity) richMessageAction
