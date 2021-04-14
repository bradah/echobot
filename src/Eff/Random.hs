{- |
Copyright:  (c) 2021 o-pascal
Maintainer: o-pascal <rtrn.0@ya.ru>

This module defines functions for random value generation.
-}

module Eff.Random
    ( -- * Random
      -- ** Instrucion set
      Random (..)
    , random
      -- Run
    , runPureRandom
    ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.State
import qualified System.Random             as R

-- | Instruction set for 'Random' effect.
data Random a where
    Random :: Random Int

-- | Get random 'Int'.
random :: Member Random r => Eff r Int
random = send Random

-- | This interpreter is entirely pure. Although it is
-- usefull for debugging since it uses some fixed seed (1st argument),
-- crappy APIs such as VK require
-- unique random values to work with them, so in those cases
-- 'IO' implementation will be more handy.
runPureRandom :: Int -> Eff (Random : State R.StdGen : r) a -> Eff r a
runPureRandom seed = evalState (R.mkStdGen seed) . interpret
    ( \case
        Random -> do
            (a, g) <- (gets @R.StdGen) R.random
            modify $ const g
            pure a
    )
