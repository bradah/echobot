module AppState where

import           Data.HashMap.Strict hiding (insert, member, (!))
import qualified Data.HashMap.Strict as Map (adjust, delete, insert, member,
                                             (!))

data AppState a = AppState
    { st'offset   :: a
    , st'sessions :: SesMap
    } deriving Show

type SesMap = HashMap Int Session

newtype Session = Session
    { ses'repNum :: Int
    } deriving Show

overSessions :: (SesMap -> SesMap) -> AppState a -> AppState a
overSessions f st = st { st'sessions = f $ st'sessions st }

newSession
    :: Int -- ^ Key
    -> Int -- ^ Value
    -> AppState a
    -> AppState a
newSession k v st = if not $ member k st
    then insert k (Session v) st
    else st

deleteSession :: Int -> AppState a -> AppState a
deleteSession = overSessions . Map.delete

member :: Int -> AppState a -> Bool
member k = Map.member k . st'sessions

insert :: Int -> Session -> AppState a -> AppState a
insert k v = overSessions (Map.insert k v)

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

getRepNum :: Int -> AppState a -> Int
getRepNum sid st = ses'repNum $ st ! sid
