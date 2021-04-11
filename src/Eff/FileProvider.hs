{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

This module defines instruction set to work with files.
-}

module Eff.FileProvider
    ( -- * FileProvider
    -- ** Instruction set
      FileProvider
    , appendFile
    , readFile
    -- ** Run
    , runIOFileProvider
    ) where

import           Control.Monad.Freer (Eff, Member, Members, interpret, send)
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO (appendFile, readFile)
import           Eff.Error           (AppError (FileProviderError), Error,
                                      IOException, throwError, try)
import           Prelude             hiding (appendFile, readFile)

-- | Instruction set for 'FileProvider' effect.
data FileProvider a where
    AppendFile :: FilePath -> Text -> FileProvider ()
    ReadFile :: FilePath -> FileProvider Text

-- | Append file.
appendFile :: Member FileProvider r
           => FilePath
           -> Text
           -> Eff r ()
appendFile path t = send $ AppendFile path t

-- | Read file content.
readFile :: Member FileProvider r
         => FilePath
         -> Eff r Text
readFile = send . ReadFile

-- | Run 'FileProvider' using 'IO'.
-- One may redefine this using pure computations to mock this effect.
runIOFileProvider
    :: Members [IO, Error AppError] r
    => Eff (FileProvider : r) a
    -> Eff r a
runIOFileProvider = interpret $ \case
    AppendFile path t -> sendOrThrow $ TIO.appendFile path t
    ReadFile path     -> sendOrThrow $ TIO.readFile path

sendOrThrow :: forall r a . Members [IO, Error AppError] r
            => IO a
            -> Eff r a
sendOrThrow io = send (try io) >>= either onFail pure
  where
    onFail :: IOException -> Eff r a
    onFail = throwError . FileProviderError
