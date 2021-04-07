module Bot.Config where

import qualified Control.Monad.Freer.Log as Log

-- TODO: FromJSON instance
data Config = Config
    { logConfig :: Log.Options
    } deriving Show
