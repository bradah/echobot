module Vk.Config where

import           Data.Aeson
import           Data.Text
import           Eff.Https  (Url)

data Config = Config
    { token          :: Text
    , groupId        :: Int
    , startMessage   :: Text
    , initialRepNum  :: Int
    , checkLpsServer :: Maybe Url
    , checkLpsKey    :: Text
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "vk" $ \o -> do
        token <- o .: "token"
        groupId <- o .: "group_id"
        startMessage <- o .: "start_message"
        initialRepNum <- o .: "initial_repeat_num"
        let checkLpsServer = Nothing
            checkLpsKey = ""
        pure Config {..}
