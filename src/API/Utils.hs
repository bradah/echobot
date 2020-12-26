{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module API.Utils where

import           Colog              (LogAction, Message)
import           Data.Text          (Text, pack)
import           Data.Text.Lazy     (toStrict)
import           Data.Typeable
import           Servant.Client     (ClientEnv (..))
import           Text.Pretty.Simple

instance Show ClientEnv where
    show (ClientEnv _ base _) = show base

instance (Typeable a, Typeable b) => Show (LogAction a b) where
    show _ = show $ typeRep (Proxy @a)

showP :: Show a => a -> Text
showP = toStrict . pShow

showT :: Show a => a -> Text
showT = pack . show
