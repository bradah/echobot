module FileProvider where

import           Control.Exception         (IOException, try)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Data.Text                 (Text)
import qualified Data.Text.IO              as T (hGetContents, hGetLine,
                                                 hPutStrLn)
import           Prelude                   hiding (readFile)
import           System.IO                 (BufferMode (..), Handle,
                                            IOMode (..), hSetBuffering)
import qualified System.IO                 as IO (hClose, openFile)

data FileProvider a where
    OpenFile :: FilePath -> IOMode -> FileProvider Handle
    HClose :: Handle -> FileProvider ()
    HGetContents :: Handle -> FileProvider Text
    HPutStrLn :: Handle -> Text -> FileProvider ()
    HGetLine :: Handle -> FileProvider Text

newtype FileProviderError = FileProviderError String
    deriving Show

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

localFileProvider
    :: Members [IO, Error FileProviderError] r
    => Eff (FileProvider : r) a
    -> Eff r a
localFileProvider = interpret $ \case
    OpenFile path mode   -> sendOrThrow $ do
        h <- IO.openFile path mode
        hSetBuffering h NoBuffering
        pure h
    HClose handle        -> sendOrThrow $ IO.hClose handle
    HGetContents handle  -> sendOrThrow $ T.hGetContents handle
    HPutStrLn handle text -> sendOrThrow $ T.hPutStrLn handle text
    HGetLine handle -> sendOrThrow $ T.hGetLine handle

sendOrThrow
    :: forall r a . Members [IO, Error FileProviderError] r
    => IO a
    -> Eff r a
sendOrThrow io = send (try io) >>= either onFail pure
  where
    onFail :: IOException -> Eff r a
    onFail = throwError . FileProviderError . show

withFile
    :: Members [IO, FileProvider, Error FileProviderError] r
    => FilePath
    -> IOMode
    -> (Handle -> Eff r a)
    -> Eff r a
withFile path mode act = do
    h <- openFile path mode
    r <- catchError (act h) (\e -> hClose h >> throwError (e :: FileProviderError))
    hClose h
    pure r

fileExample :: Eff (FileProvider : r) Text
fileExample = readFile "hie.yaml"

readFile
    :: Member FileProvider r
    => FilePath
    -> Eff r Text
readFile path = openFile path ReadMode >>= hGetContents
