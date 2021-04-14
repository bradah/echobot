{- |
Copyright:  (c) 2021 o-pascal
Maintainer: o-pascal <rtrn.0@ya.ru>

This module contains useful functions providing simple derivation of
ToJSON and FromJSON typeclasses for ADTs via Template Haskell.
-}

module TH
    ( -- * Derivation
      deriveJSON'
    , deriveToJSON'
    , deriveFromJSON'
    , snakeFieldModifier
    ) where

import           Data.Aeson.TH       (deriveFromJSON, deriveJSON, deriveToJSON)
import           Data.Aeson.Types    (Options (..), defaultOptions)
import           Data.Char           (isUpper, toLower, toUpper)
import           Data.List           (intercalate)
import           Language.Haskell.TH

-- | Convinient version of 'deriveJSON'. This function ignores
-- common prefixes in record field names and converts camelType
-- to snake_style.
deriveJSON' :: Name -> Q [Dec]
deriveJSON' name = deriveJSON (jsonOptions (nameBase name)) name

-- | Convinient version of 'deriveToJSON'. This function ignores
-- common prefixes in record field names and converts camelType
-- to snake_style.
deriveToJSON' :: Name -> Q [Dec]
deriveToJSON' name = deriveToJSON (jsonOptions (nameBase name)) name

-- | Convinient version of 'deriveFromJSON'. This function ignores
-- common prefixes in record field names and converts camelType
-- to snake_style.
deriveFromJSON' :: Name -> Q [Dec]
deriveFromJSON' name = deriveFromJSON (jsonOptions (nameBase name)) name

-- | Set of options, used in this project for toJSON and fromJSON derivation.
jsonOptions :: String -> Options
jsonOptions tname = defaultOptions
    { fieldLabelModifier     = safeTail . dropWhile (/= '\'')
    , constructorTagModifier = snakeFieldModifier tname
    , omitNothingFields      = True
    }
  where
    safeTail :: [a] -> [a]
    safeTail [] = []
    safeTail xs = tail xs

-- | This function takes care of formatting datatype's label names.
--   It essentially removes label prefix and converts it's name
--   from camelType to snake_type.
snakeFieldModifier
    :: String -- ^ Prefix, i.e. is name of datatype.
    -> String -- ^ Label name.
    -> String
snakeFieldModifier prefix xs = wordsToSnake (deleteCommonPrefixWords prefix xs)

camelWords :: String -> [String]
camelWords "" = []
camelWords s = case us of
    (_:_:_) -> us : camelWords restLs
    _       -> (us ++ ls) : camelWords rest
  where
    (us, restLs) = span  isUpper s
    (ls, rest)   = break isUpper restLs

deleteCommonPrefix :: Eq a => [a] -> [a] -> [a]
deleteCommonPrefix (x:xs) (y:ys) | x == y = deleteCommonPrefix xs ys
deleteCommonPrefix _ ys = ys

wordsToSnake :: [String] -> String
wordsToSnake = intercalate "_" . map (map toLower)

capitalise :: String -> String
capitalise (c:s) = toUpper c : s
capitalise ""    = ""

deleteCommonPrefixWords :: String -> String -> [String]
deleteCommonPrefixWords xs ys = deleteCommonPrefix
    (camelWords xs)
    (camelWords (capitalise ys))
