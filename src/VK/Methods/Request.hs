{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module VK.Methods.Request where

import           Data.Int        (Int32)
import           Data.Text       (Text)
import           GHC.Generics
import           VK.Types
import           VK.Types.Derive

data GetUpdatesResponse = GetUpdatesResponse
  { getUpdatesResponseTs      :: Maybe Ts
  , getUpdatesResponseUpdates :: [Update]
  , getUpdatesResponseFailed  :: Maybe Int32
  } deriving (Show)

type Ts = Text

deriveJSON' ''GetUpdatesResponse
