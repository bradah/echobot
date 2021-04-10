module Random where

import           Control.Monad.Freer
import           Control.Monad.Freer.State
import qualified System.Random             as R

data Random a where
    Random :: Random Int

random :: Member Random r => Eff r Int
random = send Random

runPureRandom :: Int -> Eff (Random : State R.StdGen : r) a -> Eff r a
runPureRandom seed = evalState (R.mkStdGen seed) . interpret
    ( \case
        Random -> do
            (a, g) <- (gets @R.StdGen) R.random
            modify $ const g
            pure a
    )
