module Bot.State where

import           Data.Default
import qualified Data.HashMap.Strict as Map

type SessionMap msg = Map.HashMap Int (Session msg)

data Session msg = Session
    { repNum  :: Int
    , pending :: [msg]
    } deriving (Show)

instance Default (Session msg) where
    def = Session
        { repNum = 1
        , pending = []
        }

data BotState offset msg = BotState
    { offset :: offset
    , sesMap :: SessionMap msg
    } deriving Show

getSesMap :: BotState os msg -> SessionMap msg
getSesMap = sesMap

setSesMap :: SessionMap msg -> BotState os msg -> BotState os msg
setSesMap sm st = st { sesMap = sm }

overSesMap :: (SessionMap msg -> SessionMap msg) -> BotState os msg -> BotState os msg
overSesMap f st = setSesMap (f $ getSesMap st) st

newSession :: Int -> BotState os msg -> BotState os msg
newSession k st = if not $ member k st
        then insert k def st
        else st

member ::  Int -> BotState os msg -> Bool
member k = Map.member k . getSesMap

insert :: Int -> Session msg -> BotState os msg -> BotState os msg
insert k v = overSesMap (Map.insert k v)

(!) :: BotState os msg -> Int -> Session msg
st ! k = getSesMap st Map.! k

-- | Change number of repetitions of single message for 'Conversation'.
changeRepNum
    :: Int -- ^ Session id.
    -> Int -- ^ New repetition number.
    -> BotState os msg
    -> BotState os msg
changeRepNum sid n =
    overSesMap (Map.adjust (\ses -> ses { repNum = n }) sid)

getRepNum :: Int -> BotState os msg -> Int
getRepNum sid st = repNum $ st ! sid
