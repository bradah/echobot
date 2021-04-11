{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines functions to get and work with
time on local machine.
-}

module Eff.Time
    ( -- * Time
      -- Instruction set
      Time (..)
    , getZonedTime
      -- Run
    , runIOTime
    , runPureTime
      -- Format
    , defaultTimeLocale
    , formatTime
    , ZonedTime (..)
    ) where

import           Control.Monad.Freer
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Clock                (UTCTime (..),
                                                 picosecondsToDiffTime)
import           Data.Time.Format               (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime            (ZonedTime (..),
                                                 minutesToTimeZone,
                                                 utcToZonedTime)
import qualified Data.Time.LocalTime            as T (getZonedTime)


-- | Instruction set for 'Time' effect.
data Time a where
    GetZonedTime :: Time ZonedTime

-- | Get current time on local machine.
getZonedTime :: Member Time r
             => Eff r ZonedTime
getZonedTime = send GetZonedTime

-- | Run in 'IO'.
runIOTime :: Member IO r
          => Eff (Time : r) a
          -> Eff r a
runIOTime = interpret $ \case
    GetZonedTime -> send T.getZonedTime

-- | Run purely. You cannot get genuine time of your
-- system w/o 'IO', so this is just a constant basically.
runPureTime :: forall r a
             . Eff (Time : r) a
            -> Eff r ZonedTime
runPureTime _ = pure constZonedTime
  where
    constZonedTime = utcToZonedTime timeZone $ UTCTime day diffTime
    timeZone = minutesToTimeZone 123456789

    diffTime = picosecondsToDiffTime 123456789
    day = fromOrdinalDate 2048 17
