module Vk.Config where

import           Data.Aeson
import           Data.Text
import           Https      (Url)

data Config = Config
    { token          :: Text
    , groupId        :: Int
    , checkLpsServer :: Maybe Url
    , checkLpsKey    :: Text
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "vk" $ \o -> do
        token <- o .: "token"
        groupId <- o .: "group_id"
        let checkLpsServer = Nothing
            checkLpsKey = ""
        pure Config {..}
