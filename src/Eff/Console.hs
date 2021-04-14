{- |
Copyright:  (c) 2021 o-pascal
Maintainer: o-pascal <rtrn.0@ya.ru>

Console effect.
-}

module Eff.Console
    ( -- * Console
    -- ** Instruction set
      Console (..)
    -- ** Put new line in output
    , putLine
    -- ** Run
    , runIOConsole
    , runPureConsole
    ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Writer

import           Data.Text
import           Data.Text.IO               as TIO (putStrLn)

-- | Instruction set for 'Console' effect.
data Console a where
    PutLine :: Text -> Console ()

-- | Put new line in output wich may be anything you want.
putLine :: Member Console r
        => Text
        -> Eff r ()
putLine = send . PutLine

-- | Run in 'IO'.
runIOConsole :: Member IO r
             => Eff (Console : r) a
             -> Eff r a
runIOConsole = interpret $ \case
    PutLine line -> send $ TIO.putStrLn line

-- | Run purely. Output will be collected in a list.
runPureConsole :: forall r a
                . Eff (Console : r) a
               -> Eff r (a, [Text])
runPureConsole req = runWriter (reinterpret go req)
  where
    go :: Console v -> Eff (Writer [Text] : r) v
    go (PutLine line) = tell [line]
