module Telegram.State where

import           Data.HashMap.Strict hiding (insert, member, (!))
import qualified Data.HashMap.Strict as Map (adjust, insert, member, (!))

data State = State
    { st'offset   :: Maybe Int
    , st'sessions :: SesMap
    } deriving Show

type SesMap = HashMap Int Session

newtype Session = Session
    { ses'repNum :: Int
    } deriving Show

defaultSession :: Session
defaultSession = Session
    { ses'repNum = 1
    }

defaultState :: State
defaultState = State
    { st'offset = Nothing
    , st'sessions = fromList []
    }

overSessions :: (SesMap -> SesMap) -> State -> State
overSessions f st = st { st'sessions = f $ st'sessions st }

newSession :: Int -> State -> State
newSession k st = if not $ member k st
    then insert k defaultSession st
    else st

member :: Int -> State -> Bool
member k = Map.member k . st'sessions

insert :: Int -> Session -> State -> State
insert k v = overSessions (Map.insert k v)

(!) :: State -> Int -> Session
st ! k = st'sessions st Map.! k

-- | Change number of repetitions of single message for 'Session'.
changeRepNum
    :: Int -- ^ Session id.
    -> Int -- ^ New repetition number.
    -> State
    -> State
changeRepNum sid n =
    overSessions (Map.adjust (\ses -> ses { ses'repNum = n }) sid)

getRepNum :: Int -> State -> Int
getRepNum sid st = ses'repNum $ st ! sid
