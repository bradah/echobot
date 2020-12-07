module VK.Types.Derive where

import           Data.Aeson.TH       (deriveJSON)
import           Data.Aeson.Types    (Options (..), defaultOptions)
import           Data.Char           (isUpper, toLower, toUpper)
import           Data.List           (intercalate)
import           Language.Haskell.TH

-- * Derive ToJSON and FromJSON

-- | Convinient version of deriveJSON.
deriveJSON' :: Name -> Q [Dec]
deriveJSON' name = deriveJSON (jsonOptions (nameBase name)) name

-- | Set of options, used in this project for toJSON and fromJSON derivation.
jsonOptions :: String -> Options
jsonOptions tname = defaultOptions
  { fieldLabelModifier     = snakeFieldModifier tname
  , constructorTagModifier = snakeFieldModifier tname
  , omitNothingFields      = True
  }

-- | This function takes care of formatting datatype's label names.
--   It essentially removes label prefix and converts it's name
--   from camelType to snake_type.
snakeFieldModifier
  :: String -- ^ Prefix, i.e. is name of datatype.
  -> String -- ^ Label name.
  -> String
snakeFieldModifier prefix xs =
  wordsToSnake (deleteCommonPrefixWords prefix xs)

-- ** Some utility functions

camelWords :: String -> [String]
camelWords "" = []
camelWords s
  = case us of
    (_:_:_) -> us : camelWords restLs
    _       -> (us ++ ls) : camelWords rest
  where
    (us, restLs) = span  isUpper s
    (ls, rest)   = break isUpper restLs

deleteCommonPrefix :: Eq a => [a] -> [a] -> [a]
deleteCommonPrefix (x:xs) (y:ys) | x == y =
  deleteCommonPrefix xs ys

deleteCommonPrefix _ ys = ys

wordsToSnake :: [String] -> String
wordsToSnake = intercalate "_" . map (map toLower)

capitalise :: String -> String
capitalise (c:s) = toUpper c : s
capitalise ""    = ""

deleteCommonPrefixWords :: String -> String -> [String]
deleteCommonPrefixWords xs ys =
  deleteCommonPrefix (camelWords xs) (camelWords (capitalise ys))

