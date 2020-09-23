{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module VK.Methods.Request where

import           Data.Int        (Int32)
import           Data.Text       (Text)
import           GHC.Generics
import           VK.Types
import           VK.Types.Derive

data Response = Response
  { reponseTs      :: Maybe Ts
  , reponseUpdates :: [Update]
  , responseFailed :: Int32
  } deriving (Show, Generic)

type Ts = Text

deriveJSON' ''Response
