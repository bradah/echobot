{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Generalized data structure for state maintenance.
-}

module AppState
    ( -- * AppState
    -- ** Capturing dialogues
      AppState (..)
    , SesMap
    , Session (..)
    -- ** Helper functions
    , overSessions
    , newSession
    , deleteSession
    , member
    , insert
    , (!)
    , changeRepNum
    , getRepNum
    ) where

import           Data.HashMap.Strict hiding (insert, member, (!))
import qualified Data.HashMap.Strict as Map (adjust, delete, insert, member,
                                             (!))

-- | Generalized data structure for state maintenance.
data AppState a = AppState
    { st'offset   :: a
    , st'sessions :: SesMap
    } deriving Show

-- | Session map.
type SesMap = HashMap Int Session

-- | This type represents state of bot dialog.
newtype Session = Session
    { ses'repNum :: Int
    -- ^ Number of message repetitions.
    } deriving Show

-- | Apply function over 'SesMap' contained in 'AppState'.
overSessions :: (SesMap -> SesMap) -> AppState a -> AppState a
overSessions f st = st { st'sessions = f $ st'sessions st }

-- | Add new 'Session'. This won't do anything if key is already
-- in 'SesMap'.
newSession
    :: Int -- ^ Key
    -> Int -- ^ Value
    -> AppState a
    -> AppState a
newSession k v st = if not $ member k st
    then insert k (Session v) st
    else st

-- | Delete 'Session' by key if present.
deleteSession :: Int -> AppState a -> AppState a
deleteSession = overSessions . Map.delete

-- | Check if 'AppState' already has a value with
-- specified key.
member :: Int -> AppState a -> Bool
member k = Map.member k . st'sessions

-- | Insert new 'Session'. Overrides value with the same key.
insert :: Int -> Session -> AppState a -> AppState a
insert k v = overSessions (Map.insert k v)

-- | Get 'Session' by key.
(!) :: AppState a -> Int -> Session
st ! k = st'sessions st Map.! k


-- | Change number of repetitions of single message for 'Session'.
changeRepNum
    :: Int -- ^ Session id.
    -> Int -- ^ New repetition number.
    -> AppState a
    -> AppState a
changeRepNum sid n =
    overSessions (Map.adjust (\ses -> ses { ses'repNum = n }) sid)

-- | Get repetition number.
getRepNum :: Int -> AppState a -> Int
getRepNum sid st = ses'repNum $ st ! sid
