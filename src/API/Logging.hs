{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module contains logging utilities.
-}

module API.Logging
    ( -- * Logging
      -- ** Logging functions
      logDebug
    , logInfo
    , logWarning
    , logError
    , logStdOut
    , logStdOutAndFile
    -- ** Severity
    , Severity(..)
    -- ** LogAction
    , LogAction(..)
    -- ** Message
    , Message
    ) where

import qualified Chronos                    as C
import qualified Chronos.Locale.English     as C
import           Colog                      hiding (richMessageAction,
                                             showSeverity)
import           Control.Concurrent         (ThreadId)
import           Control.Monad.IO.Class
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Lazy             (toStrict)
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Vector                as Vector
import           System.IO

-- | Log 'Message' to stdout and file.
logStdOutAndFile
    :: MonadIO m
    => Severity -- ^ Minimal 'Severity' of logging 'Message's.
    -> Handle -- ^ Handle for the log file.
    -> LogAction m Message
logStdOutAndFile sev handle = cfilter
    ((>= sev) . msgSeverity)
    (richMessageAction <> richMessageHandleNoColor handle)

-- | Log 'Message' to stdout.
logStdOut
    :: MonadIO m
    => Severity -- ^ Minimal 'Severity' of logging 'Message's.
    -> LogAction m Message
logStdOut sev = cfilter ((>= sev) . msgSeverity) richMessageAction

-- | Log 'Message' to 'Handle'
logFile
    :: MonadIO m
    => Severity -- ^ Minimal 'Severity' of logging 'Message's.
    -> Handle
    -> LogAction m Message
logFile sev handle =
    cfilter ((>= sev) . msgSeverity) $ richMessageHandleNoColor handle

-- | Log 'Message' with some additional information
-- e.g. ThreadId, Time.
richMessageAction :: MonadIO m => LogAction m Message
richMessageAction = upgradeMessageAction defaultFieldMap $
    cmapM fmtRichMessageDefault logTextStdout

-- | Same as 'richMessageAction', doesn't add colors.
richMessageHandleNoColor :: MonadIO m => Handle -> LogAction m Message
richMessageHandleNoColor handle = upgradeMessageAction defaultFieldMap $
    cmapM fmtRichMessageDefaultNoColor (logTextHandle handle)

-- | Show 'Severity' with indentation.
showSeverity :: Severity -> Text
showSeverity sev = case sev of
    Debug   -> "[Debug]   "
    Info    -> "[Info]    "
    Warning -> "[Warning] "
    Error   -> "[Error]   "

-- | Construct formatted 'Text' for 'RichMessage'.
fmtRichMessageDefaultNoColor :: MonadIO m => RichMessage m -> m Text
fmtRichMessageDefaultNoColor msg = fmtRichMessageCustomDefault msg formatRichMessage
  where
      formatRichMessage :: Maybe ThreadId -> Maybe C.Time -> Message -> Text
      formatRichMessage (maybe "" showThreadId -> thread) (maybe "" showTime -> time) Msg{..} =
           showSeverity msgSeverity
        <> time
        <> showSourceLoc msgStack
        <> thread
        <> msgText

square :: Text -> Text
square t = "[" <> t <> "] "

showThreadId :: ThreadId -> Text
showThreadId = square . T.pack . show


{- | Shows time in the following format:

>>> showTime $ C.Time 1577656800
[29 Dec 2019 22:00:00.000 +00:00]
-}
showTime :: C.Time -> Text
showTime t =
    square
    $ toStrict
    $ TB.toLazyText
    $ builderDmyHMSz (C.timeToDatetime t)

{- | Given a Datetime, constructs a 'Text' 'TB.Builder' corresponding to a
Day\/Month\/Year,Hour\/Minute\/Second\/Offset encoding of the given Datetime.

Example: @29 Dec 2019 22:00:00.000 +00:00@
-}
builderDmyHMSz :: C.Datetime -> TB.Builder
builderDmyHMSz (C.Datetime date time) =
       builderDmy date
    <> spaceSep
    <> C.builder_HMS (C.SubsecondPrecisionFixed 3) (Just ':') time
    <> spaceSep
    <> C.builderOffset C.OffsetFormatColonOn (C.Offset 0)
  where
    spaceSep :: TB.Builder
    spaceSep = TB.singleton ' '

    {- | Given a 'Date' construct a 'Text' 'TB.Builder'
    corresponding to a Day\/Month\/Year encoding.

    Example: @01 Jan 2020@
    -}
    builderDmy :: C.Date -> TB.Builder
    builderDmy (C.Date (C.Year y) m d) =
           zeroPadDayOfMonth d
        <> spaceSep
        <> TB.fromText (C.caseMonth C.abbreviated m)
        <> spaceSep
        <> TB.decimal y


    zeroPadDayOfMonth :: C.DayOfMonth -> TB.Builder
    zeroPadDayOfMonth (C.DayOfMonth d) =
        if d < 100
        then Vector.unsafeIndex twoDigitTextBuilder d
        else TB.decimal d

    twoDigitTextBuilder :: Vector.Vector TB.Builder
    twoDigitTextBuilder = Vector.fromList $
        map (TB.fromText . T.pack) twoDigitStrings
    {-# NOINLINE twoDigitTextBuilder #-}

    twoDigitStrings :: [String]
    twoDigitStrings =
        [ "00","01","02","03","04","05","06","07","08","09"
        , "10","11","12","13","14","15","16","17","18","19"
        , "20","21","22","23","24","25","26","27","28","29"
        , "30","31","32","33","34","35","36","37","38","39"
        , "40","41","42","43","44","45","46","47","48","49"
        , "50","51","52","53","54","55","56","57","58","59"
        , "60","61","62","63","64","65","66","67","68","69"
        , "70","71","72","73","74","75","76","77","78","79"
        , "80","81","82","83","84","85","86","87","88","89"
        , "90","91","92","93","94","95","96","97","98","99"
        ]
