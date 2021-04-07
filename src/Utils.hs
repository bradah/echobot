{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module contains some useful functions.
-}

module Utils
    ( -- * Utility functions
      -- ** Show pretty text.
      showP
    , showT
    , (<!>)
    ) where

import           Data.Text          (Text, pack)
import           Data.Text.Lazy     (toStrict)
import           Text.Pretty.Simple

-- | Pretty version of show for 'Text'.
showP :: Show a => a -> Text
showP = toStrict . pShowNoColor

-- | 'show' for 'Text'.
showT :: Show a => a -> Text
showT = pack . show

-- | Concat two 'Text' values with "\\n" between.
(<!>) :: Text -> Text -> Text
(<!>) a b = a <> "\n" <> b
