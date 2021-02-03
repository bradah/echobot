{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module contains some useful functions.
-}

module API.Utils
    ( -- * Utility functions
      -- ** Show pretty text.
      showP
    , showT
    , (<!>)
    ) where

import           Colog              (LogAction)
import           Data.Text          (Text, pack)
import           Data.Text.Lazy     (toStrict)
import           Servant.Client     (ClientEnv (..))
import           Text.Pretty.Simple

instance Show ClientEnv where
    show (ClientEnv _ base _) = show base

instance Show (LogAction a b) where
    show _ = "LogAction"

-- | Pretty version of show for 'Text'.
showP :: Show a => a -> Text
showP = toStrict . pShowNoColor

-- | 'show' for 'Text'.
showT :: Show a => a -> Text
showT = pack . show

-- | Concat two 'Text' values with "\n" between.
(<!>) :: Text -> Text -> Text
(<!>) a b = a <> "\n" <> b
