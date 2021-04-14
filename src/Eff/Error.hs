{- |
Copyright:  (c) 2021 o-pascal
Maintainer: o-pascal <rtrn.0@ya.ru>

This module defines simple error throwing and catching effect.
-}
module Eff.Error
    ( -- * Error
      -- ** AppError
      AppError (..)
    , module M
    ) where

import           Control.Exception         as M
import           Control.Monad.Freer.Error as M
import           Data.Text                 (Text)
import           Network.HTTP.Req          as M (HttpException)

-- | This type represents all possible kinds of errors in
-- this application.
data AppError
    = FileProviderError
        IOException
    | ConfiguratorError
        { path        :: FilePath
        , description :: String
        }
    | HttpsError
        HttpException
    | OtherError
        Text
    deriving Show
