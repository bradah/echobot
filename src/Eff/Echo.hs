{- |
Copyright:  (c) 2021 wspbr
Maintainer: wspbr <rtrn.0@ya.ru>

Echo effect.
-}

module Eff.Echo
    ( -- * Echo
    -- ** Instruction set
      Echo (..)
    , listen
    , reply
    -- ** Bot loop
    , echo
    ) where

import           Control.Monad       (forever)
import           Control.Monad.Freer

-- | Instruction set for 'Echo' effect.
data Echo u a where
    Listen :: Echo u [u]
    Reply :: u -> Echo u ()

-- | Listen for updates.
listen
    :: Member (Echo u) r
    => Eff r [u]
listen = send Listen

-- | Reply to update.
reply
    :: Member (Echo u) r
    => u
    -> Eff r ()
reply = send . Reply

-- | Echobot life cycle. Define an interpreter for your
-- platform to run it.
echo
    :: forall u r . Member (Echo u) r
    => Eff r [u]
echo = forever $ listen @u >>= mapM_ reply
