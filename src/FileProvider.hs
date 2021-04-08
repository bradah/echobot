module FileProvider where

import           Control.Monad.Freer (Eff, Member, Members, interpret, send)
import           Data.Text           (Text)
import qualified Data.Text.IO        as T (hGetContents, hGetLine, hPutStrLn)
import           Error               (AppError (FileProviderError), Error,
                                      IOException, catchError, throwError, try)
import           Prelude             hiding (readFile)
import           System.IO           (BufferMode (..), Handle, IOMode (..),
                                      hSetBuffering)
import qualified System.IO           as IO (hClose, openFile)

data FileProvider a where
    OpenFile :: FilePath -> IOMode -> FileProvider Handle
    HClose :: Handle -> FileProvider ()
    HGetContents :: Handle -> FileProvider Text
    HPutStrLn :: Handle -> Text -> FileProvider ()
    HGetLine :: Handle -> FileProvider Text

openFile
    :: Member FileProvider r
    => FilePath
    -> IOMode
    -> Eff r Handle
openFile path mode = send $ OpenFile path mode

hClose
    :: Member FileProvider r
    => Handle
    -> Eff r ()
hClose = send . HClose

hGetContents
    :: Member FileProvider r
    => Handle
    -> Eff r Text
hGetContents = send . HGetContents

hPutStrLn
    :: Member FileProvider r
    => Handle
    -> Text
    -> Eff r ()
hPutStrLn h t = send $ HPutStrLn h t

hGetLine
    :: Member FileProvider r
    => Handle
    -> Eff r Text
hGetLine = send . HGetLine

runIOFileProvider
    :: Members [IO, Error AppError] r
    => Eff (FileProvider : r) a
    -> Eff r a
runIOFileProvider = interpret $ \case
    OpenFile path mode   -> sendOrThrow $ do
        h <- IO.openFile path mode
        hSetBuffering h NoBuffering
        pure h
    HClose h        -> sendOrThrow $ IO.hClose h
    HGetContents h  -> sendOrThrow $ T.hGetContents h
    HPutStrLn h text -> sendOrThrow $ T.hPutStrLn h text
    HGetLine h -> sendOrThrow $ T.hGetLine h

sendOrThrow
    :: forall r a . Members [IO, Error AppError] r
    => IO a
    -> Eff r a
sendOrThrow io = send (try io) >>= either onFail pure
  where
    onFail :: IOException -> Eff r a
    onFail = throwError . FileProviderError

withFile
    :: Members [FileProvider, Error AppError] r
    => FilePath
    -> IOMode
    -> (Handle -> Eff r a)
    -> Eff r a
withFile path mode act = do
    h <- openFile path mode
    r <- catchError (act h) (\e -> hClose h >> throwError (e :: AppError))
    hClose h
    pure r

readFile
    :: Member FileProvider r
    => FilePath
    -> Eff r Text
readFile path = openFile path ReadMode >>= hGetContents
