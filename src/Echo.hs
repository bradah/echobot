module Echo where

import           Control.Monad       (forever)
import           Control.Monad.Freer

data Echo a where
    Listen :: Echo [a]
    Reply :: a -> Echo b


listen
    :: Member Echo r
    => Eff r [e]
listen = send Listen

reply
    :: Member Echo r
    => a
    -> Eff r b
reply = send . Reply

echo
    :: Member Echo r
    => Eff r e
echo = forever $ listen >>= mapM reply
