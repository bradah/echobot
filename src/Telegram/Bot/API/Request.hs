{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Request where

import           Data.Aeson
import           Data.Text                            (Text)
import           GHC.Generics
import           Telegram.Bot.API.Internal.Derivation

-- * Types representing Telegram response

-- | Bot token
type Token = Text

-- | Telegram response
data Response a = Response
  { responseOk          :: Bool
  , responseDescription :: Maybe Text
  , responseResult      :: a
  , responseErrorCode   :: Maybe Int
  } deriving (Show, Generic)

deriveJSON' ''Response
