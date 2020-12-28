{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module API.Utils where

import           Colog              (LogAction)
import           Data.Text          (Text, pack)
import           Data.Text.Lazy     (toStrict)
import           Servant.Client     (ClientEnv (..))
import           Text.Pretty.Simple

instance Show ClientEnv where
    show (ClientEnv _ base _) = show base

instance Show (LogAction a b) where
    show _ = "LogAction"

showP :: Show a => a -> Text
showP = toStrict . pShow

showT :: Show a => a -> Text
showT = pack . show
