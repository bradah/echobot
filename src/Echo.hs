module Echo where

import           Control.Monad       (forever)
import           Control.Monad.Freer


data Echo u a where
    Listen :: Echo u [u]
    Reply :: u -> Echo u ()


listen
    :: Member (Echo u) r
    => Eff r [u]
listen = send Listen

reply
    :: Member (Echo u) r
    => u
    -> Eff r ()
reply = send . Reply

echo
    :: forall u r . Member (Echo u) r
    => Eff r [u]
echo = forever $ listen @u >>= mapM_ reply
