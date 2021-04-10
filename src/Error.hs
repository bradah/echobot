module Error
    ( AppError (..)
    , module M
    ) where

import           Control.Exception         as M
import           Control.Monad.Freer.Error as M
import           Data.Text                 (Text)
import           Network.HTTP.Req          as M (HttpException)
import           Vk.Requests               as M (GetLpsError)

data AppError
    = FileProviderError
        IOException
    | ConfiguratorError
        { path        :: FilePath
        , description :: String
        }
    | HttpsError
        HttpException
    | VkError
        GetLpsError
    | OtherError
        Text
    deriving Show
